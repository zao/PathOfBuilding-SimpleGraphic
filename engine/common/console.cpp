// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Console
//

#include "common.h"

#include <charconv>
#include <deque>
#include <string>
#include <string_view>
#include <fmt/format.h>

// =============
// Configuration
// =============

constexpr size_t CON_MAXLINES = 1024;

constexpr size_t CON_MAXCMDBUFFER = 1024;	// Maximum buffered commands

constexpr size_t CON_MAXHIST = 32;			// Maximum history

// ==========
// Cvar Class
// ==========

conVar_c::conVar_c(IConsole* conHnd)
	: con(conHnd)
{}

conVar_c::~conVar_c()
{}

void conVar_c::Set(int in)
{
	Set(fmt::format("{}", in).c_str());
}

void conVar_c::Set(float in)
{
	Set(fmt::format("{:f}", in).c_str());
}

void conVar_c::Set(char const* in)
{
	if (in == strVal) {
		// No change
		return;
	}

	mod = true;			// Flag as modified
	std::from_chars(in, in + strlen(in), intVal); // Set values
	floatVal = (float)strtod(in, nullptr);
	strVal = in;

	Clamp();			// Clamp value
}

void conVar_c::Toggle()
{
	Set(!intVal);
}

bool conVar_c::GetMod()
{
	bool wasMod = mod;
	mod = false;
	return wasMod;
}

void conVar_c::Reset()
{
	intVal = atoi(defVal.c_str());
	floatVal = (float)atof(defVal.c_str());
	strVal = defVal;
}

void conVar_c::Clamp()
{
	if ((flags & CV_CLAMP) == 0) {
		return;
	}

	if (intVal < min) {
		Set(min);
	} else if (intVal > max) {
		Set(max);
	} else {
		return;
	}
	
	con->Print(fmt::format("\"{}\" clamped to {}\n", name, intVal).c_str());
}

// =======
// Classes
// =======

// Buffer line
struct conLine_s {
	std::string buf;
	bool newLine = false;
};

// Print hook list entry
struct conHookEntry_s {
	conHookEntry_s* prev;
	conHookEntry_s* next;
	conPrintHook_c* hook;
};

// ==================
// IConsole Interface
// ==================

class console_c: public IConsole {
public:
	// Interface
	void	Print(std::string_view text);
	void	PrintFunc(std::string_view func);
	void	Warning(std::string_view text);
	void	Clear();
	void	Scroll(int mode);
	std::optional<std::string_view> EnumLines(int* index);
	std::string BuildBuffer();

	void	Execute(std::string_view cmd);
	void	ExecCommands(bool deferUnknown);

	conVar_c* Cvar_Add(std::string_view name, int flags, std::string_view def, int minVal = 0, int maxVal = 0);
	conVar_c* Cvar_Ptr(std::string_view name);

	conCmd_c* EnumCmd(int* index);
	conVar_c* EnumCvar(int* index);

	// Encapsulated
	console_c();
	~console_c();

	std::deque<conLine_s> bufLines;
	int		bufScroll;	// Scroll point

	void	Buffer_Init();
	void	Buffer_Shutdown();
	void	Buffer_PrintLine(std::string_view text);

	conHookEntry_s* hookFirst;
	conHookEntry_s* hookLast;

	void	Hook_RunHooks(std::string_view text);
	void	Hook_RunClear();

	std::vector<conCmd_c> cmdList;
	conCmd_c* Cmd_Ptr(std::string_view name);

	std::vector<std::unique_ptr<conVar_c>> cvarList;
	int Cvar_Find(std::string_view name);

	std::vector<std::string> cmdBuf_lines;

	textBuffer_c input;				// Input buffer
	std::deque<textBuffer_c> hist;	// Command history buffers
	int		histSel;				// Current selection in history
};

InterfacePtr<IConsole> IConsole::GetHandle()
{
	return std::make_unique<console_c>();
}

console_c::console_c()
{
	Buffer_Init();

	// Initialise hooks
	hookFirst = NULL;
	hookLast = NULL;

	histSel = -1;
}

console_c::~console_c()
{
	// Clear command buffer
	cmdBuf_lines.clear();

	// Delete commands and variables
	cmdList.clear();
	cvarList.clear();

	// Delete hooks
	conHookEntry_s* i = hookFirst;
	while (i) {
		delete LL_Next(i);
	}

	Buffer_Shutdown();
}

// ===========================
// Console Buffer and Printing
// ===========================

void console_c::Buffer_Init()
{
	bufLines.emplace_back();
	bufScroll = 0;
}

void console_c::Buffer_Shutdown()
{
	bufLines.clear();
}

void console_c::Buffer_PrintLine(std::string_view text)
{
	conLine_s* line{};
	if (!bufLines.back().newLine) {
		line = &bufLines.back();
	}
	else {
		if (bufLines.size() == CON_MAXLINES) {
			bufLines.pop_front();
		}
		line = &bufLines.emplace_back();
	}

	line->buf += text;
}

void console_c::Print(std::string_view text)
{
	// Run print hooks
	Hook_RunHooks(text);

	std::string_view p = text;
	while (p.size()) {
		if (const auto newlineIdx = p.find('\n'); newlineIdx != p.npos) {
			Buffer_PrintLine(p.substr(0, newlineIdx));
			bufLines.back().newLine = true;
			p = p.substr(newlineIdx + 1);
		}
		else {
			Buffer_PrintLine(p);
			p = {};
		}
	}

	// Scroll to the bottom
	bufScroll = bufLines.size() - 1;
}

void console_c::PrintFunc(std::string_view func)
{
	// Print function title
	Print(fmt::format("\n--- {} ---\n", func).c_str());
}

void console_c::Warning(std::string_view text)
{
	Print(fmt::format("^4Warning: {}\n", text));
}

void console_c::Clear()
{
	// Recreate output buffer and run hook clears
	Buffer_Shutdown();
	Buffer_Init();
	Hook_RunClear();
}

void console_c::Scroll(int mode)
{
	switch (mode) {
	case CBSC_UP:
		// Try to scroll up 4 lines
		bufScroll = std::max(0, bufScroll - 4);
		break;
	case CBSC_DOWN:
		// Try to scroll down 4 lines
		bufScroll = std::min((int)bufLines.size() - 1, bufScroll + 4);
		break;
	case CBSC_BOTTOM:
		// Scroll to last line in buffer
		bufScroll = bufLines.size() - 1;
		break;
	}
}

std::optional<std::string_view> console_c::EnumLines(int* index)
{
	if (*index <= -1) {
		// Start traversing from scroll point
		*index = bufScroll;
	} else if (*index == 0) {
		// Reached the end of the buffer
		return std::nullopt;
	} else {
		*index = *index - 1;
	}
	
	// Return next line
	return bufLines[*index].buf;
}

std::string console_c::BuildBuffer()
{
	fmt::memory_buffer buf;

	// Append the lines
	for (const auto& line : bufLines) {
		fmt::format_to(fmt::appender(buf), "{}\n", line.buf);
	}

	return to_string(buf);
}

// =====================
// Console Print Hooking
// =====================

conPrintHook_c::conPrintHook_c(IConsole* conHnd)
{
	_con = (console_c*)conHnd;
}

conPrintHook_c::~conPrintHook_c()
{
	RemovePrintHook();
}

void conPrintHook_c::InstallPrintHook()
{
	RemovePrintHook();
	conHookEntry_s* h = new conHookEntry_s;
	h->hook = this;
	LL_Link(_con->hookFirst, _con->hookLast, h);
}

void conPrintHook_c::RemovePrintHook()
{
	conHookEntry_s* i = _con->hookFirst;
	while (i) {
		if (i->hook == this) {
			LL_Unlink(_con->hookFirst, _con->hookLast, i);
			delete i;
			return;
		}
		LL_Next(i);
	}
}

void console_c::Hook_RunHooks(std::string_view text)
{
	conHookEntry_s* i = hookFirst;
	while (i) {
		LL_Next(i)->hook->ConPrintHook(text);
	}
}

void console_c::Hook_RunClear()
{
	conHookEntry_s* i = hookFirst;
	while (i) {
		LL_Next(i)->hook->ConPrintClear();
	}
}

// ================
// Console Commands
// ================

conCmdHandler_c::conCmdHandler_c(IConsole* conHnd)
{
	_con = (console_c*)conHnd;
}

conCmdHandler_c::~conCmdHandler_c()
{
	// Remove any commands added by this handler
	auto& seq = _con->cmdList;
	seq.erase(std::remove_if(seq.begin(), seq.end(), [this](const conCmd_c& cmd) {
		return cmd.obj == this;
	}), seq.end());
}

void conCmdHandler_c::Cmd_PrivAdd(const char* name, int minArgs, const char* usage, conCmdHandler_c* obj, conCmdMethod_t method)
{
	if (_con->Cmd_Ptr(name)) {
		_con->Warning(fmt::format("command '{}' already exists", name));
		return;
	}

	// Find a free slot
	_con->cmdList.emplace_back(conCmd_c{name, minArgs, usage, obj, method});
}

conCmd_c* console_c::Cmd_Ptr(std::string_view name)
{
	std::string name_str(name);
	const auto it = std::find_if(cmdList.begin(), cmdList.end(), [name](const conCmd_c& cmd) { return cmd.name == name; });
	if (it != cmdList.end()) {
		return &*it;
	}
	return nullptr;
}

conCmd_c* console_c::EnumCmd(int* index)
{
	if (*index < -1 || *index >= cmdList.size() - 1) {
		return NULL;
	}
	while (1) {
		(*index)++;
		if (*index >= cmdList.size()) {
			return NULL;
		}
		return &cmdList[*index];
	}
}

// =================
// Console Variables
// =================

conVar_c* console_c::Cvar_Add(std::string_view name, int flags, std::string_view def, int minVal, int maxVal)
{
	std::optional<std::string> setVal;
	int slot = Cvar_Find(name);
	if (slot >= 0) {
		if (cvarList[slot]->flags & CV_SET) {
			// Has been set, take value and delete old cvar
			setVal = cvarList[slot]->strVal;
			cvarList[slot].reset();
		} else {
			return cvarList[slot].get();
		}
	} else {
		// Find a free slot
		slot = cvarList.size();
		cvarList.emplace_back();
	}

	cvarList[slot] = std::make_unique<conVar_c>(this);
	cvarList[slot]->name = (std::string)name;
	cvarList[slot]->flags = flags;
	cvarList[slot]->defVal = (std::string)def;
	if (flags & CV_CLAMP) {
		cvarList[slot]->min = minVal;
		cvarList[slot]->max = maxVal;
	}
	cvarList[slot]->Reset();

	if (setVal) {
		cvarList[slot]->Set(setVal->c_str());
		cvarList[slot]->mod = false;
	}

	return cvarList[slot].get();
}

conVar_c* console_c::Cvar_Ptr(std::string_view name)
{
	int slot = Cvar_Find(name);
	if (slot >= 0) {
		return cvarList[slot].get();
	} else {
		return NULL;
	}
}

int console_c::Cvar_Find(std::string_view name)
{
	std::string name_str(name);
	int slot = 0;
	// Find the cvar and return the index
	for (const auto& cv : cvarList) {
		if (cv && cv->name == name)
			return slot;
		++slot;
	}
	return -1;
}

conVar_c* console_c::EnumCvar(int* index)
{
	if (*index < -1 || *index >= cvarList.size() - 1) {
		return nullptr;
	}
	while (1) {
		(*index)++;
		if (*index >= cvarList.size()) {
			return nullptr;
		}
		if (cvarList[*index]) {
			return cvarList[*index].get();
		}
	}
}

// ===============
// String Executor
// ===============

void console_c::Execute(std::string_view cmd)
{
	std::string_view newCmd = cmd;
	std::string_view sep = ";\n";
	while (!newCmd.empty()) {
		auto end = newCmd.find_first_of(sep);
		if (end == newCmd.npos) {
			end = newCmd.size();
		}
		std::string lp(newCmd.substr(0, end));
		newCmd = newCmd.substr(end);

		cmdBuf_lines.push_back(lp);
	}
}

void console_c::ExecCommands(bool deferUnknown)
{
	std::vector<std::string> deferred;
	for (auto& cmdLine : cmdBuf_lines) {
		// Split command string
		args_c args(cmdLine.c_str());
		if (args.argc == 0) {
			continue;
		}

		// Check for commands first
		conCmd_c* cmd = Cmd_Ptr(args[0]);
		if (cmd) {
			if (args.argc < cmd->minArgs + 1) {
				// Too few arguments
				Print(fmt::format("Usage: {} {}\n", cmd->name, cmd->usage).c_str());
			} else {
				// We've got arguments, or the command doesn't care
				(cmd->obj->*cmd->method)(this, args);
			}
		} else {
			conVar_c* cv = Cvar_Ptr(args[0]);
			if (cv) {
				if (args.argc >= 2) {
					// There are arguments, try and set cvar
					if (cv->flags & CV_READONLY) {
						Print(fmt::format("'{}' is read only.\n", cv->name).c_str());
					} else {
						cv->Set(args[1]);
					}
				} else {
					// No arguments, so print current value
					Print(fmt::format("'{}' is: \"{}\" default: \"{}\"\n", cv->name, cv->strVal, cv->defVal).c_str());
				}
			} else if (deferUnknown) {
				// Defer execution of unknown commands
				deferred.emplace_back(std::move(cmdLine));
			} else {
				Print(fmt::format("Unknown command '{}'\n", args[0]).c_str());
			}
		}
	}

	cmdBuf_lines = std::move(deferred);
}

// =============
// Console Input
// =============

conInputHandler_c::conInputHandler_c(IConsole* conHnd)
{
	_con = (console_c*)conHnd;
}

void conInputHandler_c::ClearConInput()
{
	_con->histSel = -1;		
	_con->input.Init();

	RefreshConInput();
}

void conInputHandler_c::RefreshConInput()
{
	SetConInput(_con->input.buf, _con->input.caret);
}

void conInputHandler_c::ConInputKeyEvent(int key, int type)
{
	if (type == KE_KEYDOWN) {
		switch (key) {
		// PgUp/Dn and mousewheel scroll the buffer
		case KEY_PGUP:
		case KEY_MWHEELUP:
			_con->Scroll(CBSC_UP);
			return;
		case KEY_PGDN:
		case KEY_MWHEELDOWN:
			_con->Scroll(CBSC_DOWN);
			return;
		// Up/down select from command history
		case KEY_UP:
			if (_con->histSel < _con->hist.size()) {
				// Copy next history item
				_con->histSel++;
				_con->input = _con->hist[_con->histSel].buf;
				RefreshConInput();
			}
			return;
		case KEY_DOWN:
			if (_con->histSel > 0) {
				// Copy previous history item
				_con->histSel--;
				_con->input = _con->hist[_con->histSel].buf;
				RefreshConInput();
			} else if (_con->histSel == 0) {
				// Clear buffer if we go after history start
				ClearConInput();
			}
			return;
		// Tab completes or finds matches for input buffer text
		case KEY_TAB:
			if (_con->input.len) {
				std::string comp = _con->input.buf;
				int	compLen = comp.size();

				// Build match list
				struct Match
				{
					std::string match;
					std::string args;
				};
				std::vector<Match> matches;
				for (const auto& cmd : _con->cmdList) {
					if (std::string_view(cmd.name).substr(0, comp.size()) == comp) {
						matches.emplace_back(Match{cmd.name, cmd.usage});
					}
				}
				for (const auto& cv : _con->cvarList) {
					if (cv && std::string_view(cv->name).substr(0, comp.size()) == comp) {
						matches.emplace_back(Match{cv->name, fmt::format("= \"{}\"", cv->strVal)});
					}
				}

				if (matches.size()) {
					// Matches were found
					if (matches.size()  == 1) {
						// Exact match
						comp = matches[0].match;
						if (!matches[0].args.empty()) {
							comp += " ";
						}
					} else {
						size_t minMatchLen = ~0;
						// Multiple matches, print them out
						_con->Print(fmt::format("]{}\n", comp).c_str());
						for (const auto& m : matches) {
							_con->Print(fmt::format("  {} {}\n", m.match, m.args).c_str());
							minMatchLen = (std::min)(minMatchLen, m.match.size());
						}

						// Try to refine comparison string
						comp.clear();
						for (size_t compIdx = 0; compIdx < minMatchLen; ++compIdx) {
							char c = matches[0].match[compIdx];
							bool fail = false;
							for (int m = 1; m < matches.size(); m++) {
								if (matches[m].match[compIdx] != c) {
									fail = true;
									break;
								}
							}
							if (fail) {
								break;
							}
							comp += c;
						}
					}

					// Copy comparison string back into input buffer
					_con->input = comp;
					RefreshConInput();
				}
			}
			return;
		// Return executes input buffer
		case KEY_RETURN:
			if (_con->input.len) {
				// Execute buffer
				_con->Print(fmt::format("]{}\n", _con->input.buf));
				_con->Execute(_con->input.buf);

				// Add to command history if different from most recent command
				if (_con->hist.empty() || _stricmp(_con->hist[0].buf, _con->input.buf)) {
					if (_con->hist.size() == CON_MAXHIST)
						_con->hist.pop_back();
					_con->hist.emplace_front() = _con->input.buf;
				}

				// Clear the input buffer
				ClearConInput();
			}
			return;
		}
	}

	// Send other key events to the input buffer
	if (_con->input.KeyEvent(key, type)) {
		RefreshConInput();
	}
}
