// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// System Video Header
//

#include <glm/vec2.hpp>

// =======
// Classes
// =======

// Video settings flags
enum vidFlags_e {
	VID_RESIZABLE = 0x04,
	VID_MAXIMIZE = 0x08,
	VID_USESAVED = 0x10,
};

// Render API
enum class sys_vidApi_e {
	ANGLE,
	DX11,
};

// Saved video state structure
struct sys_vidSave_s {
	glm::ivec2 size{};
	glm::ivec2 pos{};
	int maximised = false;
	glm::ivec2 fbSize{};
	float dpiScale = 1.0f;
	sys_vidApi_e api{};
};

// Video settings structure
struct sys_vidSet_s {
	bool	shown = false;	// Show window?
	int		flags = 0;		// Flags
	glm::ivec2 mode{};		// Window size
	glm::ivec2 minSize{};	// Minimum size for resizable windows
	sys_vidSave_s save;		// Saved state
	std::optional<sys_vidApi_e> api; // Preferred render API
};

// ==========
// Interfaces
// ==========

// System Video
class sys_IVideo {
public:
	static InterfacePtr<sys_IVideo> GetHandle(class sys_IMain* sysHnd);
	virtual ~sys_IVideo() = default;

	sys_vidSave_s vid;	// Current state

	virtual	int		Apply(sys_vidSet_s* set) = 0;	// Apply settings

	virtual void	SetForeground() = 0; // Activate the window if shown
	virtual bool	IsActive() = 0; // Get activated status
	virtual void	FramebufferSizeChanged(int width, int height) = 0; // Respond to framebuffer size change
	virtual void	SizeChanged(int width, int height, bool max) = 0; // Respond to window size change
	virtual void	PosChanged(int x, int y) = 0; // Respond to window position change
	virtual void	GetMinSize(int &width, int &height) = 0; // Get minimum window size
	virtual void	SetVisible(bool vis) = 0;		// Show/hide window
	virtual bool	IsVisible() = 0; // Get whether the window is shown
	virtual void	SetTitle(std::u8string_view title) = 0;// Change window title
	virtual void*	GetWindowHandle() = 0;			// Get window handle
	virtual void	GetRelativeCursor(int &x, int &y) = 0; // Get cursor position relative to window
	virtual void	SetRelativeCursor(int x, int y) = 0; // Set cursor position relative to window
	virtual bool	IsCursorOverWindow() = 0; // Get whether the cursor is over the window, including obstructions
};
