// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// System Main Header
//

#include <filesystem>
#include <optional>
#include <string>

// =======
// Classes
// =======

// Timer
class timer_c {
public:
	timer_c();
	void	Start();
	int		Get();
private:
	std::chrono::system_clock::time_point startTime;
};

// Thread
class thread_c {
public:
	thread_c(class sys_IMain* sys);
	void	ThreadStart(bool lowPri = false);
private:
	class sys_main_c* _sysMain;
	virtual void ThreadProc() = 0;
	static unsigned long statThreadProc(void* obj);
};

// File finder
class find_c {
public:
	std::filesystem::path fileName;
	bool	isDirectory = false;
	uintmax_t	fileSize = 0;
	unsigned long long modified = 0;

	find_c();
	~find_c();
	bool	FindFirst(std::filesystem::path const&& fileSpec);
	bool	FindNext();
private:
	std::optional<std::u8string> globPattern; // Empty pattern accepts all files like "*" and "*.*"
	std::filesystem::directory_iterator iter;
};

std::tuple<std::string, std::string> GetWineHostVersion();

// ==========
// Interfaces
// ==========

// System Main
class sys_IMain {
public:
	InterfacePtr<IConsole> con;
	InterfacePtr<sys_IConsole> conWin;
	InterfacePtr<sys_IVideo> video;

	bool		x64 = false;
	bool		debug = false;
	bool		debuggerRunning = false;
	int			processorCount = 0;
	std::filesystem::path basePath;
	std::optional<std::filesystem::path> userPath;
	std::optional<std::string> userPathReason;

	virtual int		GetTime() = 0;
	virtual void	Sleep(int msec) = 0;
	virtual bool	IsKeyDown(byte key) = 0;
	virtual void	ClipboardCopy(const char8_t* str) = 0;
	virtual std::optional<std::u8string> ClipboardPaste() = 0;
	virtual bool	SetWorkDir(std::filesystem::path const& newCwd = {}) = 0;
	virtual void	SpawnProcess(std::filesystem::path cmdName, const char8_t* argList) = 0;
	virtual std::optional<std::u8string> OpenURL(const char8_t* url) = 0;
	virtual void	Error(const char8_t* fmt, ...) = 0;
	virtual void	Exit(std::u8string_view msg = {}) = 0;
	virtual void	Restart() = 0;
};
