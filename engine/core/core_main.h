// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Engine Core Header
//

// ==========
// Interfaces
// ==========

// Engine Core
class core_IMain {
public:
	static InterfacePtr<core_IMain> GetHandle(sys_IMain* sysHnd);
	virtual ~core_IMain() = default;

	InterfacePtr<core_IConfig> config;
	InterfacePtr<core_IVideo> video;

	virtual void	Init(int argc, char** argv) = 0;
	virtual void	Frame() = 0;
	virtual void	Shutdown() = 0;
	virtual void	KeyEvent(int key, int type) = 0;
	virtual bool	CanExit() = 0;
};

