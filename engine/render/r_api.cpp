#include "r_local.h"

r_api_c::r_api_c(r_renderer_c* renderer)
	: renderer(renderer)
	, sys(renderer->sys)
{
}
