#pragma once
#include "r_api.h"

std::shared_ptr<r_api_c> MakeDirectXRendererAPI(class r_renderer_c* renderer);
