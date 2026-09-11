// DyLua: SimpleGraphic
// (c) David Gowor, 2014
//
// Module: UI Sub Script
//

#include "ui_local.h"

#include <memory>
#include <sstream>
#include <string>
#include <thread>
#include <variant>

#include <readerwriterqueue/readerwriterqueue.h>

// =======
// Classes
// =======

struct ssTweenNil_s {};
using ssTweenBoolean_t = bool;
using ssTweenNum_t = lua_Number;
using ssTweenString_t = std::string;

using ssTweenData_t = std::variant<ssTweenNil_s, ssTweenBoolean_t, ssTweenNum_t, ssTweenString_t>;

struct ssCall_s {
	std::string name;
	std::vector<ssTweenData_t> data;
};

// =======================
// ui_ISubScript Interface
// =======================

class ui_subscript_c: public ui_ISubScript {
public:
	// Interface
	bool	Start();
	void	SubScriptFrame();
	bool	IsRunning();
	size_t	GetScriptMemory();

	// Encapsulated
	ui_subscript_c(ui_main_c* ui, dword id);
	~ui_subscript_c();

	ui_main_c* ui = nullptr;
	dword	id = 0;

	std::jthread worker;

	lua_State* L = nullptr;
	bool running = false;
	std::atomic_bool finished;
	moodycamel::ReaderWriterQueue<ssCall_s> subCalls;

	struct WorkerMainFinished {};
	struct WorkerMainFuncCall {
		ssCall_s funcCall;
	};
	struct WorkerMainSubCall {
		ssCall_s subCall;
	};

	using WorkerMainMessage = std::variant<WorkerMainFinished, WorkerMainFuncCall, WorkerMainSubCall>;
	moodycamel::BlockingReaderWriterQueue<WorkerMainMessage> workerMainQueue;
	moodycamel::BlockingReaderWriterQueue<std::vector<ssTweenData_t>> funcRet;

	std::optional<std::string> errorStr;

	void	Stop();

	void	ThreadProc();

	void	LAssert(int cond, const char* fmt, ...);
};

ui_ISubScript* ui_ISubScript::GetHandle(ui_main_c* ui, dword id)
{
	return new ui_subscript_c(ui, id);
}

void ui_ISubScript::FreeHandle(ui_ISubScript* hnd)
{
	delete (ui_subscript_c*)hnd;
}

ui_subscript_c::ui_subscript_c(ui_main_c* ui, dword id)
	: ui(ui), id(id), funcRet(1)
{
	L = NULL;
	running = false;
}

ui_subscript_c::~ui_subscript_c()
{
	Stop();

	if (L) {
		lua_close(L);
	}
}

// =======================
// Lua Interface Utilities
// =======================

static ui_subscript_c* GetSSPtr(lua_State* L)
{
	lua_rawgeti(L, LUA_REGISTRYINDEX, 0);
	ui_subscript_c* ss = (ui_subscript_c*)lua_touserdata(L, -1);
	lua_pop(L, 1);
	return ss;
}

void ui_subscript_c::LAssert(int cond, const char* fmt, ...)
{
	if ( !cond ) {
		va_list va;
		va_start(va, fmt);
		lua_pushvfstring(L, fmt, va);
		va_end(va);
		lua_error(L);
	}
}

// From lua.c
static int traceback (lua_State *L) {
  if (!lua_isstring(L, 1))  /* 'message' not a string? */
    return 1;  /* keep it intact */
  lua_getglobal(L, "debug");
  if (!lua_istable(L, -1)) {
    lua_pop(L, 1);
    return 1;
  }
  lua_getfield(L, -1, "traceback");
  if (!lua_isfunction(L, -1)) {
    lua_pop(L, 2);
    return 1;
  }
  lua_pushvalue(L, 1);  /* pass error message */
  lua_pushinteger(L, 2);  /* skip this function and traceback */
  lua_call(L, 2, 1);  /* call debug.traceback */
  return 1;
}

static int l_panicFunc(lua_State* L)
{
	ui_subscript_c* ss = GetSSPtr(L);
	ss->ui->sys->Error("Unprotected Lua error:\n%s", lua_tostring(L, -1));
	return 0;
}

// ==================
// Tween Data Helpers
// ==================

static std::vector<ssTweenData_t> ssBuildData(lua_State* L, int start)
{
	std::vector<ssTweenData_t> ret;
	int n = lua_gettop(L);
	for (int a = start; a <= n; a++) {
		switch (lua_type(L, a)) {
		case LUA_TNIL:
			ret.emplace_back(ssTweenNil_s{});
			break;
		case LUA_TBOOLEAN:
			ret.emplace_back(ssTweenBoolean_t{lua_toboolean(L, a) != 0});
			break;
		case LUA_TNUMBER:
			ret.emplace_back(ssTweenNum_t{lua_tonumber(L, a)});
			break;
		case LUA_TSTRING: {
			size_t len{};
			const char* ptr = lua_tolstring(L, a, &len);
			ret.emplace_back(ssTweenString_t(ptr, len));
			break;
		}
		}
	}
	lua_settop(L, start - 1);
	return ret;
}

namespace
{
	struct Pusher
	{
		lua_State* L;

		void operator() (ssTweenNil_s) const { lua_pushnil(L); }
		void operator() (ssTweenBoolean_t b) const { lua_pushboolean(L, b ? 1 : 0); }
		void operator() (ssTweenNum_t num) const { lua_pushnumber(L, num); }
		void operator() (const ssTweenString_t& str) { lua_pushlstring(L, str.c_str(), str.size()); }
	};
}

static int ssPushData(lua_State* L, std::vector<ssTweenData_t>&& list)
{
	Pusher pusher{L};
	int numdat = list.size();
	for (auto& data : list) {
		lua_checkstack(L, 1);
		std::visit(pusher, data);
	}
	list.clear();
	return numdat;
}

// ============================
// Sub Script API and Utilities
// ============================

static int l_SubScriptFunc(lua_State* L)
{
	ui_subscript_c* ss = GetSSPtr(L);
	int n = lua_gettop(L);
	const char* funcName = lua_tostring(L, lua_upvalueindex(1)); 
	for (int i = 1; i <= n; i++) {
		ss->LAssert(lua_isnil(L, i) || lua_isboolean(L, i) || lua_isnumber(L, i) || lua_isstring(L, i),
			"%s() argument %d: only nil, boolean, number and string can be passed to the main script", funcName, i);
	}

	// Call
	ss->workerMainQueue.emplace(ui_subscript_c::WorkerMainFuncCall{{
		.name = funcName,
		.data = ssBuildData(L, 1),
	}});

	// Response
	std::vector<ssTweenData_t> retData;
	ss->funcRet.wait_dequeue(retData);
	return ssPushData(L, std::move(retData));
}

static int l_SubScriptSub(lua_State* L)
{
	ui_subscript_c* ss = GetSSPtr(L);
	int n = lua_gettop(L);
	const char* subName = lua_tostring(L, lua_upvalueindex(1));
	for (int i = 1; i <= n; i++) {
		ss->LAssert(lua_isnil(L, i) || lua_isboolean(L, i) || lua_isnumber(L, i) || lua_isstring(L, i),
			"%s() argument %d: only nil, boolean, number and string can be passed to the main script", subName, i);
	}
	ss->workerMainQueue.emplace(ui_subscript_c::WorkerMainSubCall{{
		.name = subName,
		.data = ssBuildData(L, 1),
	}});
	return 0;
}

static int l_os_exit(lua_State* L)
{
	return 0;
}

static void parseSubScriptList(lua_State* L, const char* clist, lua_CFunction func)
{
	std::istringstream iss(clist);
	std::string tok;
	while (std::getline(iss, tok, ',')) {
		lua_pushstring(L, tok.c_str());
		lua_pushcclosure(L, func, 1);
		lua_setglobal(L, tok.c_str());
	}
}

static void l_hookStop(lua_State* L, lua_Debug* dbg)
{
	lua_pushstring(L, "dummy");
	lua_error(L);
}

// ===================
// UI Sub Script Class
// ===================

bool ui_subscript_c::Start()
{
	finished = false;

	// Initialise Lua
	L = luaL_newstate();
	if ( !L ) return false;
	lua_atpanic(L, l_panicFunc);
	lua_pushlightuserdata(L, this);
	lua_rawseti(L, LUA_REGISTRYINDEX, 0);
	lua_pushcfunction(L, traceback);

#ifdef _WIN32
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, "LUA_NOENV");
#endif

	// Add libraries and APIs
	lua_gc(L, LUA_GCSTOP, 0);
	luaL_openlibs(L);
	lua_getglobal(L, "os");
	lua_pushcfunction(L, l_os_exit);
	lua_setfield(L, -2, "exit");
	lua_pop(L, 1);
	parseSubScriptList(L, lua_tostring(ui->L, 2), l_SubScriptFunc);
	parseSubScriptList(L, lua_tostring(ui->L, 3), l_SubScriptSub);
	lua_gc(L, LUA_GCRESTART, -1);

	// Load the script
	int err = luaL_loadstring(L, lua_tostring(ui->L, 1));
	if (err) {
		lua_pushstring(ui->L, lua_tostring(L, -1));
		lua_error(ui->L);
	}

	// Copy arguments and launch script thread
	lua_pushinteger(L, ssPushData(L, ssBuildData(ui->L, 4)));
	worker = std::jthread(&ui_subscript_c::ThreadProc, this);
	running = true;

	return true;
}

void ui_subscript_c::Stop()
{
	if (running) {
		// Set hook to stop script on the next line
		lua_sethook(L, l_hookStop, LUA_MASKLINE, 0);
		// We're either waiting for the thread to finish or for a function call to be pending.
		WorkerMainMessage workerMainMessage{};
		while (true) {
			workerMainQueue.wait_dequeue(workerMainMessage);
			if (std::holds_alternative<WorkerMainFinished>(workerMainMessage)) {
				break;
			}
			else if (std::holds_alternative<WorkerMainFuncCall>(workerMainMessage)) {
				// Script is waiting on function call; discard to let it make progress
				funcRet.emplace();
			}
			else if (std::holds_alternative<WorkerMainSubCall>(workerMainMessage)) {
				// Ignore sub calls
			}
		}
	}
}

void ui_subscript_c::ThreadProc()
{
	int numarg = (int)lua_tointeger(L, -1);
	lua_pop(L, 1);
	if (lua_pcall(L, numarg, LUA_MULTRET, 1)) {
		const auto* err = lua_tostring(L, -1);
		errorStr = err ? std::optional{std::string(err)} : std::nullopt;
	}
	workerMainQueue.emplace(WorkerMainFinished{});
}

void ui_subscript_c::SubScriptFrame()
{
	if (!running)
		return;
	bool didFinish = false;
	{
		WorkerMainMessage workerMainMessage{};
		while (workerMainQueue.try_dequeue(workerMainMessage)) {
			if (std::get_if<WorkerMainFinished>(&workerMainMessage)) {
				didFinish = true;
			}
			else if (auto* subCall = std::get_if<WorkerMainSubCall>(&workerMainMessage)) {
				auto& call = subCall->subCall;
				int extraArgs = ui->PushCallback("OnSubCall");
				if (extraArgs >= 0) {
					// Run the main script
					lua_pushstring(ui->L, call.name.c_str());
					int numdat = ssPushData(ui->L, std::move(call.data));
					ui->PCall(extraArgs + numdat + 1, 0);
				}
			}
			else if (auto* funcCall = std::get_if<WorkerMainFuncCall>(&workerMainMessage)) {
				auto& call = funcCall->funcCall;
				// Process function call
				int retStart = lua_gettop(ui->L) + 1;
				bool doRet = false;
				int extraArgs = ui->PushCallback("OnSubCall");
				if (extraArgs >= 0) {
					// Run the main script
					lua_pushstring(ui->L, call.name.c_str());
					int numdat = ssPushData(ui->L, std::move(call.data));
					ui->PCall(extraArgs + numdat + 1, LUA_MULTRET);
					doRet = true;

					// Validate return value types
					int n = lua_gettop(ui->L);
					for (int i = retStart; i <= n; i++) {
						if (!(lua_isnil(ui->L, i) || lua_isboolean(ui->L, i) || lua_isnumber(ui->L, i) || lua_isstring(ui->L, i))) {
							char* msg = AllocStringLen(128);
							sprintf(msg, "OnSubCall() return %d: only nil, boolean, number and string can be returned to sub script", i - retStart + 1);
							ui->DoError("Runtime error in", msg);
							FreeString(msg);
							doRet = false;
						}
					}
				}
				if (doRet) {
					// Grab return values from main script
					funcRet.emplace(ssBuildData(ui->L, retStart));
				}
				else {
					funcRet.emplace();
				}
			}
		}
	}

	if (didFinish) {
		running = false;
		if (errorStr) {
			int extraArgs = ui->PushCallback("OnSubError");
			if (extraArgs >= 0) {
				lua_pushlightuserdata(ui->L, (void*)(uintptr_t)id);
				lua_pushstring(ui->L, errorStr->c_str());
				ui->PCall(extraArgs + 2, 0);
			}
		} else {
			int extraArgs = ui->PushCallback("OnSubFinished");
			if (extraArgs >= 0) {
				// Validate return value types
				int n = lua_gettop(L);
				for (int i = 2; i <= n; i++) {
					if ( !(lua_isnil(L, i) || lua_isboolean(L, i) || lua_isnumber(L, i) || lua_isstring(L, i)) ) {
						char* msg = AllocStringLen(128);
						sprintf(msg, "Subscript return %d: only nil, boolean, number and string can be returned from sub script", i - 1);
						ui->DoError("Runtime error in", msg);
						FreeString(msg);
						lua_settop(L, 1);
						break;
					}
				}
				lua_pushlightuserdata(ui->L, (void*)(uintptr_t)id);
				ui->PCall(extraArgs + 1 + ssPushData(ui->L, ssBuildData(L, 2)), 0);
			}
		}
	}
}

bool ui_subscript_c::IsRunning()
{
	return running;
}

size_t ui_subscript_c::GetScriptMemory()
{
	return running? lua_gc(L, LUA_GCCOUNT, 0) : 0;
}
