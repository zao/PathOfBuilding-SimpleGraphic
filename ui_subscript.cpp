// DyLua: SimpleGraphic
// (c) David Gowor, 2014
//
// Module: UI Sub Script
//

#include "ui_local.h"

#include <latch>
#include <variant>

#include <moodycamel/concurrentqueue.h>
#include <readerwriterqueue/readerwriterqueue.h>

// =======
// Classes
// =======

struct ssTweenData_s {
	std::unique_ptr<ssTweenData_s> next;
	using Value = std::variant<std::monostate, bool, double, std::u8string>;
	Value value;
};

struct ssCall_s {
	std::u8string name;
	std::unique_ptr<ssTweenData_s> data;
};

// =======================
// ui_ISubScript Interface
// =======================

class ui_subscript_c: public ui_ISubScript, public thread_c {
public:
	// Interface
	bool	Start();
	void	SubScriptFrame();
	bool	IsRunning();
	size_t	GetScriptMemory();

	// Encapsulated
	ui_subscript_c(ui_main_c* ui, uintptr_t id);
	~ui_subscript_c();

	ui_main_c* ui = nullptr;
	uintptr_t id = 0;

	lua_State* L = nullptr;
	std::atomic<bool> running = false;
	std::latch finished{1};
	
	moodycamel::BlockingReaderWriterQueue<ssCall_s> subCalls{32};

	moodycamel::BlockingReaderWriterQueue<ssCall_s> funcCall{1};
	moodycamel::BlockingReaderWriterQueue<std::unique_ptr<ssTweenData_s>> funcReturn{1};

	std::optional<std::u8string> errorStr;

	void	Stop();

	void	ThreadProc();

	void	LAssert(int cond, const char* fmt, ...);
};

InterfacePtr<ui_ISubScript> ui_ISubScript::GetHandle(ui_main_c* ui, uintptr_t id)
{
	return std::make_unique<ui_subscript_c>(ui, id);
}

ui_subscript_c::ui_subscript_c(ui_main_c* ui, uintptr_t id)
	: thread_c(ui->sys), ui(ui), id(id)
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
	ss->ui->sys->Error(u8"Unprotected Lua error:\n%s", lua_tostring(L, -1));
	return 0;
}

// ==================
// Tween Data Helpers
// ==================

static std::unique_ptr<ssTweenData_s> ssBuildData(lua_State* L, int start)
{
	std::unique_ptr<ssTweenData_s> ret;
	ssTweenData_s* last{};
	int n = lua_gettop(L);
	for (int a = start; a <= n; a++) {
		auto d = std::make_unique<ssTweenData_s>();
		switch (lua_type(L, a)) {
		case LUA_TNIL:
			break;
		case LUA_TBOOLEAN:
			d->value.emplace<bool>(lua_toboolean(L, a) != 0);
			break;
		case LUA_TNUMBER:
			d->value.emplace<double>(lua_tonumber(L, a));
			break;
		case LUA_TSTRING:
			d->value.emplace<std::u8string>((const char8_t*)lua_tostring(L, a));
			break;
		}
		if (last) {
			last->next = std::move(d);
			last = last->next.get();
		}
		else {
			ret = std::move(d);
			last = ret.get();
		}
	}
	lua_settop(L, start - 1);
	return ret;
}

static int ssPushData(lua_State* L, std::unique_ptr<ssTweenData_s> list)
{
	int numdat = 0;
	for ( ; list; numdat++) {
		const auto data = std::move(list);
		lua_checkstack(L, 1);
		if (std::holds_alternative<std::monostate>(data->value)) {
			lua_pushnil(L);
		}
		else if (const auto* val = std::get_if<bool>(&data->value)) {
			lua_pushboolean(L, *val);
		}
		else if (const auto* val = std::get_if<double>(&data->value)) {
			lua_pushnumber(L, *val);
		}
		else if (const auto* val = std::get_if<std::u8string>(&data->value)) {
			lua_pushstring(L, (const char*)val->c_str());
		}
		list = std::move(data->next);
	}
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
	ssCall_s call{};
	call.name = (const char8_t*)funcName;
	call.data = ssBuildData(L, 1);
	ss->funcCall.emplace(std::move(call));

	std::unique_ptr<ssTweenData_s> callRet;
	ss->funcReturn.wait_dequeue(callRet);
	return ssPushData(L, std::move(callRet));
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
	ssCall_s call{};
	call.name = (const char8_t*)subName;
	call.data = ssBuildData(L, 1);
	ss->subCalls.emplace(std::move(call));
	return 0;
}

static int l_os_exit(lua_State* L)
{
	return 0;
}

static void parseSubScriptList(lua_State* L, const char* clist, lua_CFunction func)
{
	char* list = AllocString(clist);
	char* tok = strtok(list, ",");
	while (tok) {
		lua_pushstring(L, tok);
		lua_pushcclosure(L, func, 1);
		lua_setglobal(L, tok);
		tok = strtok(NULL, ",");
	}
	delete list;
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
	ThreadStart();
	running = true;

	return true;
}

void ui_subscript_c::Stop()
{
	ssCall_s call;
	if (running) {
		// Set hook to stop script on the next line
		lua_sethook(L, l_hookStop, LUA_MASKLINE, 0);
		while ( !finished.try_wait() && !funcCall.try_dequeue(call)) ui->sys->Sleep(0);
	}

	if (call.data) {
		// Script is waiting on function call; discard and wait for script to stop
		funcReturn.emplace();
		finished.wait();
	}

	// Discard data for any pending sub calls
	while (subCalls.pop()) {}
}

void ui_subscript_c::ThreadProc()
{
	PerformanceAPI_SetCurrentThreadName("Subscript");
	int numarg = (int)lua_tointeger(L, -1);
	lua_pop(L, 1);
	if (lua_pcall(L, numarg, LUA_MULTRET, 1)) {
		errorStr = (const char8_t*)lua_tostring(L, -1);
	}
	finished.count_down();
}

void ui_subscript_c::SubScriptFrame()
{
	bool didFinish = finished.try_wait();
	if (running) {
		// Check for sub calls
		ssCall_s call;
		while (subCalls.try_dequeue(call)) {
			int extraArgs = ui->PushCallback("OnSubCall");
			if (extraArgs >= 0) {
				// Run the main script
				lua_pushstring(ui->L, (const char*)call.name.c_str());
				int numdat = ssPushData(ui->L, std::move(call.data));
				ui->PCall(extraArgs + numdat + 1, 0);
			}
			call.data.reset();
		}
		
		if (funcCall.try_dequeue(call)) {
			// Process function call
			int retStart = lua_gettop(ui->L) + 1;
			bool doRet = false;
			int extraArgs = ui->PushCallback("OnSubCall");
			if (extraArgs >= 0) {
				// Run the main script
				lua_pushstring(ui->L, (const char*)call.name.c_str());
				int numdat = ssPushData(ui->L, std::move(call.data));
				ui->PCall(extraArgs + numdat + 1, LUA_MULTRET);
				doRet = true;

				// Validate return value types
				int n = lua_gettop(ui->L);
				for (int i = retStart; i <= n; i++) {
					if ( !(lua_isnil(ui->L, i) || lua_isboolean(ui->L, i) || lua_isnumber(ui->L, i) || lua_isstring(ui->L, i)) ) {
						const auto msg = fmt::format(u8"OnSubCall() return {}: only nil, boolean, number and string can be returned to sub script", i - retStart + 1);
						ui->DoError(u8"Runtime error in", msg);
						doRet = false;
					}
				}
			}

			std::unique_ptr<ssTweenData_s> callRet;
			if (doRet) {
				// Grab return values from main script
				callRet = ssBuildData(ui->L, retStart);
			}
			funcReturn.emplace(std::move(callRet));
		}
	}
	if (didFinish) {
		running = false;
		if (errorStr) {
			int extraArgs = ui->PushCallback("OnSubError");
			if (extraArgs >= 0) {
				lua_pushlightuserdata(ui->L, (void*)id);
				lua_pushstring(ui->L, errorStr ? (const char*)errorStr->c_str() : "");
				ui->PCall(extraArgs + 2, 0);
			}
			errorStr.reset();
		} else {
			int extraArgs = ui->PushCallback("OnSubFinished");
			if (extraArgs >= 0) {
				// Validate return value types
				int n = lua_gettop(L);
				for (int i = 2; i <= n; i++) {
					if ( !(lua_isnil(L, i) || lua_isboolean(L, i) || lua_isnumber(L, i) || lua_isstring(L, i)) ) {
						const auto msg = fmt::format(u8"Subscript return {}: only nil, boolean, number and string can be returned from sub script", i - 1);
						ui->DoError(u8"Runtime error in", msg);
						lua_settop(L, 1);
						break;
					}
				}
				lua_pushlightuserdata(ui->L, (void*)id);
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
