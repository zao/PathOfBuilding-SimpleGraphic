// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Core Video Header
//

// ==========
// Interfaces
// ==========

// Video Manager
class core_IVideo {
public:
	static InterfacePtr<core_IVideo> GetHandle(sys_IMain* sysHnd);
	virtual ~core_IVideo() = default;

	virtual void	Apply(bool shown = true) = 0;
	virtual void	Save() = 0;
};
