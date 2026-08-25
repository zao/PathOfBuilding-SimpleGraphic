// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// System Local Header
// Platform: Windows
//

#include "common.h"

#include "system.h"

#ifdef _WIN32
#define _WIN32_WINNT _WIN32_WINNT_WIN7
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <shellapi.h>
#include <mmsystem.h>
#endif

#include <chrono>
#include <vector>

// =======
// Classes
// =======

// System Main: sys_main.cpp
class sys_main_c: public sys_IMain {
public:
	// Interface
	int		GetTime();
	void	Sleep(int msec);
	bool	IsKeyDown(byte key);
	void	ClipboardCopy(const char8_t* str);
	std::optional<std::u8string> ClipboardPaste();
	bool	SetWorkDir(std::filesystem::path const& newCwd = {});
	void	SpawnProcess(std::filesystem::path cmdName, const char8_t* argList);
	std::optional<std::u8string> OpenURL(const char8_t* url); // return value has failure reason
	void	Error(const char8_t* fmt, ...);
	void	Exit(const char8_t* msg = NULL);
	void	Restart();

	// Encapsulated
	sys_main_c();
	~sys_main_c(); // Out of line so that `core`'s dtor can see a complete type

	bool	Run(int argc, char** argv);

	byte	GlfwKeyToKey(int key, int scancode);
	char	GlfwKeyExtraChar(int key);

#ifdef _WIN32
	HINSTANCE hinst = nullptr;
	HICON	icon = nullptr;
#endif

	InterfacePtr<class core_IMain> core;

	bool	initialised = false;
	bool	minimized = false;
	volatile bool	exitFlag = false;
	volatile bool	restartFlag = false;
	std::optional<std::u8string> exitMsg;
	std::optional<std::u8string> threadError;
	bool	errorRaised = false;
	std::chrono::system_clock::time_point baseTime;
	std::vector<uint8_t> heldKeyState;
};
