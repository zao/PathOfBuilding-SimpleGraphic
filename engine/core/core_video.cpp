// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Core Video
//

#include "common.h"
#include "system.h"

#include "core_video.h"

#include <algorithm>
#include <ranges>

// =====================
// core_IVideo Interface
// =====================

class core_video_c: public core_IVideo, public conCmdHandler_c {
public: 
	// Interface
	void	Apply(bool shown = true);
	void	Save();

	// Encapsulated
	core_video_c(sys_IMain* sysHnd);

	sys_IMain* sys;

	conVar_c* vid_resizable;
	conVar_c* vid_last;
	conVar_c* vid_api;
};

InterfacePtr<core_IVideo> core_IVideo::GetHandle(sys_IMain* sysHnd)
{
	return std::make_unique<core_video_c>(sysHnd);
}

core_video_c::core_video_c(sys_IMain* sysHnd)
	: conCmdHandler_c(sysHnd->con.get()), sys(sysHnd)
{
	vid_resizable	= sys->con->Cvar_Add(u8"vid_resizable", CV_ARCHIVE|CV_CLAMP, CFG_VID_DEFRESIZABLE, 0, 3);
	vid_last		= sys->con->Cvar_Add(u8"vid_last", CV_ARCHIVE, u8"");
	vid_api			= sys->con->Cvar_Add(u8"vid_api", CV_ARCHIVE, u8"");
}

// =============
// Video Manager
// =============

void core_video_c::Apply(bool shown)
{
	// Apply video settings
	sys_vidSet_s set;
	set.shown = shown;
	set.flags = 0;
	if (vid_resizable->intVal) {
		set.flags|= VID_RESIZABLE;
		if (vid_resizable->intVal == 2) {
			set.flags|= VID_MAXIMIZE;
		} else if (vid_resizable->intVal == 3) {
			if (sscanf((const char*)vid_last->strVal.c_str(), "%d,%d,%d,%d,%d", &set.save.size.x, &set.save.size.y, &set.save.pos.x, &set.save.pos.y, (int*)&set.save.maximised) == 5) {
				// Clamp saved window size as it may be persisted as zero before.
				set.save.size[0] = (std::max)(CFG_VID_MINWIDTH, set.save.size[0]);
				set.save.size[1] = (std::max)(CFG_VID_MINHEIGHT, set.save.size[1]);
				set.flags|= VID_USESAVED;
			} else {
				set.flags|= VID_MAXIMIZE;
			}
		}
	}
	set.mode[0] = 0;
	set.mode[1] = 0;
	set.minSize[0] = CFG_VID_MINWIDTH;
	set.minSize[1] = CFG_VID_MINHEIGHT;
	if (const auto& apiStr = vid_api->strVal; !apiStr.empty()) {
		for (const auto& [from, to] : apiStr | std::views::split(',')) {
			std::u8string_view cand(from, to);
#if SIMPLEGRAPHIC_HAVE_ANGLE
			if (CaseInsensitiveEqual(cand, u8"angle"sv)) {
				set.api = sys_vidApi_e::ANGLE;
				break;
			}
#endif
#if SIMPLEGRAPHIC_HAVE_DIRECTX
			if (CaseInsensitiveEqual(cand, u8"dx11"sv)) {
				set.api = sys_vidApi_e::DX11;
				break;
			}
#endif
#if SIMPLEGRAPHIC_HAVE_WEBGPU
			if (CaseInsensitiveEqual(cand, u8"wgpu"sv)) {
				set.api = sys_vidApi_e::WebGPU;
				break;
			}
#endif
		}
	}
	sys->video->Apply(&set);
}

void core_video_c::Save()
{
	// Save video size/pos if needed
	if (vid_resizable->intVal == 3) {
		char spec[64];
		sprintf(spec, "%d,%d,%d,%d,%d", sys->video->vid.size.x, sys->video->vid.size.y, sys->video->vid.pos.x, sys->video->vid.pos.y, sys->video->vid.maximised);
		vid_last->Set((const char8_t*)spec);
	}
}
