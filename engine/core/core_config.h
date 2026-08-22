// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Core Config Header
//

// ==========
// Interfaces
// ==========

// Core Config
class core_IConfig {
public:
	static InterfacePtr<core_IConfig> GetHandle(sys_IMain* sysHnd);
	virtual ~core_IConfig() = default;
	
	virtual bool	LoadConfig(std::filesystem::path const& cfgName) = 0;
	virtual bool	SaveConfig(std::filesystem::path const& cfgName) = 0;
};
