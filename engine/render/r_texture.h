// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Render Texture Header
//

// =======
// Classes
// =======

#include <atomic>
#include <memory>
#include <mutex>
#include <string>

class image_c;
class mip_set_c;

// Texture
class r_tex_c : public std::enable_shared_from_this<r_tex_c> {
	struct CreateToken {};

	void Kick();

public:
	r_tex_c(CreateToken, BorrowedInterfacePtr<class r_ITexManager> manager, std::u8string_view fileName, int flags);
	r_tex_c(CreateToken, BorrowedInterfacePtr<class r_ITexManager> manager, std::unique_ptr<image_c> img, int flags);
	~r_tex_c();

	int		error;
	enum Status
	{
		INIT,
		IN_QUEUE,
		PROCESSING,
		SIZE_KNOWN,
		PENDING_UPLOAD,
		DONE,
	};
	mutable std::mutex statusMutex;
	mutable std::condition_variable statusCV;
	std::atomic<Status> status;
	std::atomic<int> loadPri;
	dword	texId;
	int		flags;
	std::u8string fileName;
	std::atomic<dword> fileWidth;
	std::atomic<dword> fileHeight;
	std::unique_ptr<image_c> img;
	GLenum target{};
	size_t stackLayers = 1;

	static std::shared_ptr<r_tex_c> CreateFromPath(BorrowedInterfacePtr<class r_ITexManager> manager, std::u8string_view fileName, int flags);
	static std::shared_ptr<r_tex_c> CreateFromImage(BorrowedInterfacePtr<class r_ITexManager> manager, std::unique_ptr<image_c> img, int flags);

	void Bind();
	void Unbind();
	void Enable();
	void Disable();

	void AbortLoad();
	void LoadFile();

	[[nodiscard]] Status GetStatus() const noexcept;
	void SetStatus(Status newStatus);
	void WaitOnStatusAtLeast(Status bound) const noexcept;

	void PerformUpload();

private:
	class t_manager_c* manager;
	class r_renderer_c* renderer;
	void	Init(BorrowedInterfacePtr<class r_ITexManager> manager, std::u8string_view fileName, int flags);
	void	Upload(image_c& img, int flags);
	std::unique_ptr<image_c> BuildMipSet(std::unique_ptr<image_c> img);
};

// ==========
// Interfaces
// ==========

// Texture Manager
class r_ITexManager {
public:
	static InterfacePtr<r_ITexManager> GetHandle(r_renderer_c* renderer);
	virtual ~r_ITexManager() = default;

	virtual int		GetAsyncCount() = 0;
	virtual void	ProcessPendingTextureUploads() = 0;
};
