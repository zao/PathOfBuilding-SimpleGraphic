// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Core Config
//

#include "common.h"
#include "system.h"

#include "core_config.h"

#include <fmt/core.h>
#include <fmt/ostream.h>
#include <fstream>
#include <gsl/span>

// =======================
// core_IConfig Interface
// =======================

class core_config_c: public core_IConfig, public conPrintHook_c, public conCmdHandler_c {
public:
	// Interface
	bool	LoadConfig(std::filesystem::path const& cfgName);
	bool	SaveConfig(std::filesystem::path const& cfgName);

	// Encapsulated
	core_config_c(sys_IMain* sysHnd);

	sys_IMain* sys;

	void	C_Set(IConsole* conHnd, args_c &args);
	void	C_SetA(IConsole* conHnd, args_c &args);
	void	C_Toggle(IConsole* conHnd, args_c &args);
	void	C_CmdList(IConsole* conHnd, args_c &args);
	void	C_CvarList(IConsole* conHnd, args_c &args);
	void	C_Clear(IConsole* conHnd, args_c &args);
	void	C_Exec(IConsole* conHnd, args_c &args);
	void	C_Exit(IConsole* conHnd, args_c &args);
	void	C_Restart(IConsole* conHnd, args_c &args);
	void	C_MemReport(IConsole* conHnd, args_c &args);

	conVar_c* con_log;
	bool	logOpen;
	std::ofstream logFile;

	void	ConPrintHook(const char* text);
};

core_IConfig* core_IConfig::GetHandle(sys_IMain* sysHnd)
{
	return new core_config_c(sysHnd);
}

void core_IConfig::FreeHandle(core_IConfig* hnd)
{
	delete (core_config_c*)hnd;
}

core_config_c::core_config_c(sys_IMain* sysHnd)
	: conPrintHook_c(sysHnd->con), conCmdHandler_c(sysHnd->con), sys(sysHnd)
{
	Cmd_Add("set", 2, "<cvar_name> <cvar_value>", this, &core_config_c::C_Set);
	Cmd_Add("seta", 2, "<cvar_name> <cvar_value>", this, &core_config_c::C_SetA);
	Cmd_Add("toggle", 1, "<cvar_name>", this, &core_config_c::C_Toggle);
	Cmd_Add("cmdList", 0, "", this, &core_config_c::C_CmdList);
	Cmd_Add("cvarList", 0, "", this, &core_config_c::C_CvarList);
	Cmd_Add("clear", 0, "", this, &core_config_c::C_Clear);
	Cmd_Add("exec", 1, "<configname>", this, &core_config_c::C_Exec);
	Cmd_Add("exit", 0, "", this, &core_config_c::C_Exit);
	Cmd_Add("quit", 0, "", this, &core_config_c::C_Exit);
	Cmd_Add("restart", 0, "", this, &core_config_c::C_Restart);
	Cmd_Add("memreport", 0, "", this, &core_config_c::C_MemReport);

	con_log = sys->con->Cvar_Add("con_log", CV_ARCHIVE, "0");
	logOpen = false;
	InstallPrintHook();
}

// ==============
// Basic Commands
// ==============

void core_config_c::C_Set(IConsole* conHnd, args_c &args)
{
	conVar_c* cv = conHnd->Cvar_Add(args[1], CV_SET, "");
	cv->Set(args[2]);
}

void core_config_c::C_SetA(IConsole* conHnd, args_c &args)
{
	conVar_c* cv = conHnd->Cvar_Add(args[1], CV_SET|CV_ARCHIVE, "");
	cv->Set(args[2]);
}

void core_config_c::C_Toggle(IConsole* conHnd, args_c &args)
{
	conVar_c* cv = conHnd->Cvar_Ptr(args[1]);
	if (cv) {
		// Toggle it
		cv->Toggle();
	} else {
		// Oops.
		conHnd->Printf("Cvar '%s' does not exist.\n", args[1]);
	}
}

void core_config_c::C_CmdList(IConsole* conHnd, args_c &args)
{
	int index = -1;
	while (conCmd_c* cmd = conHnd->EnumCmd(&index)) {
		conHnd->Print(fmt::format(" {} {}\n", cmd->name, cmd->usage).c_str());
	}
}

void core_config_c::C_CvarList(IConsole* conHnd, args_c &args)
{
	int index = -1;
	while (conVar_c* cv = conHnd->EnumCvar(&index)) {
		conHnd->Print(fmt::format("{}{}{}  {} = \"{}\"\n", cv->flags & CV_ARCHIVE? 'A':' ', cv->flags & CV_READONLY? 'R':' ', cv->flags & CV_CLAMP? 'C':' ', cv->name, cv->strVal).c_str());
	}
}

void core_config_c::C_Clear(IConsole* conHnd, args_c &args)
{
	conHnd->Clear();
}

void core_config_c::C_Exec(IConsole* conHnd, args_c &args)
{
	LoadConfig(args[1]);
}

void core_config_c::C_Exit(IConsole* conHnd, args_c &args)
{
	sys->Exit();
}

void core_config_c::C_Restart(IConsole* conHnd, args_c &args)
{
	sys->Restart();
}

void core_config_c::C_MemReport(IConsole* conHnd, args_c &args)
{
#ifdef _MEMTRAK_H
	_memTrak_memReport("memreport.log");
	conHnd->Printf("Memory report saved to memreport.log\n");
#else
	conHnd->Printf("Memory report not available in Release builds.\n");
#endif
}

// ============
// Config Files
// ============

bool core_config_c::LoadConfig(std::filesystem::path const& cfgName)
{
	// Make sure it has .cfg extension
	auto fileName = cfgName;
	fileName.replace_extension(".cfg");

	sys->con->Print(fmt::format("Executing {}\n", fileName.generic_u8string()).c_str());

	// Read the config file
	std::ifstream f(fileName, std::ios::binary);
	if (!f) {
		sys->con->Warning("config file not found");
		return false;
	}
	auto cfg = SlurpFile(fileName, 1).value();
	cfg.back() = '\n';

	// Parse the config text

	gsl::span<const char> rest(cfg);
	while (rest.size()) {
		// Find end of line
		const auto n = std::find_if(rest.begin(), rest.end(), [](char ch) { return ch == '\r' || ch == '\n'; }) - rest.begin();
		auto line = rest.subspan(0, n);
		rest = rest.subspan(n + 1);

		// Remove line comments
		if (auto it = std::adjacent_find(line.begin(), line.end(), [](char a, char b) { return a == '/' && b == '/'; }); it != line.end()) {
			line = line.subspan(0, it - line.begin());
		}

		// Execute if there's anything left
		if (line.size()) {
			sys->con->Execute(std::string_view(line.data(), line.size()));
		}
	}

	return true;
}

bool core_config_c::SaveConfig(std::filesystem::path const& cfgName)
{
	// Make sure it has .cfg extension
	auto fileName = cfgName;
	fileName.replace_extension(".cfg");

	sys->con->Print(fmt::format("Saving {}\n", fileName.generic_u8string()).c_str());

	// Open the config file
	std::ofstream f(fileName);
	if (!f) {
		sys->con->Warning("couldnt write config file");
		return false;
	}

	// Write archived cvars
	int index = -1;
	while (conVar_c* cv = sys->con->EnumCvar(&index)) {
		if (cv->flags & CV_ARCHIVE) {
			fmt::println(f, "set {} \"{}\"", cv->name, cv->strVal);
		}
	}

	return true;
}

// ===============
// Console Logging
// ===============

void core_config_c::ConPrintHook(const char* text)
{
	if (con_log->intVal) {
		if (logOpen == false) {
			logFile.open(std::filesystem::u8path(CFG_LOGFILE));
			logOpen = true;
			fmt::println(logFile, "Log opened.");
		}
		logFile.write(text, strlen(text));
		logFile.flush();
	} else {
		if (logOpen) {
			logFile.close();
			logOpen = false;
		}
	}
}
