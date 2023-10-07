// DyLua: SimpleGraphic
// (c) David Gowor, 2014
//
// Module: UI API
//

#include "ui_local.h"

#include <array>
#include <filesystem>
#include <fmt/core.h>
#include <glm/mat3x3.hpp>
#include <glm/vec3.hpp>
#include <numeric>
#include <zlib.h>

/* OnFrame()
** OnChar("<char>")
** OnKeyDown("<keyName>")
** OnKeyUp("<keyName>")
** canExit = CanExit()
** OnExit()
** OnSubCall("<name>", ...)
** OnSubError(ssID, "<errorMsg>")
** OnSubFinished(ssID, ...)
**
** SetCallback("<name>"[, func])
** func = GetCallback("<name>")
** SetMainObject(object)
**
** imgHandle = NewImageHandle()
** imgHandle:Load("<fileName>"[, "flag1"[, "flag2"...]])  flag:{"ASYNC"|"CLAMP"|"MIPMAP"}
** imgHandle:Unload()
** isValid = imgHandle:IsValid()
** isLoading = imgHandle:IsLoading()
** imgHandle:SetLoadingPriority(pri)
** width, height = imgHandle:ImageSize()
**
** mshHandle = NewMeshHandle(coords, texcoords[, indices])
**
** RenderInit()
** width, height = GetScreenSize()
** SetClearColor(red, green, blue[, alpha])
** SetDrawLayer({layer|nil}[, subLayer)
** GetDrawLayer()
** SetViewport([x, y, width, height])
** SetDrawColor(red, green, blue[, alpha]) / SetDrawColor("<escapeStr>")
** DrawImage({imgHandle|nil}, left, top, width, height[, tcLeft, tcTop, tcRight, tcBottom])
** DrawImageAt({imgHandle|nil}, xform, left, top, width, height[, tcLeft, tcTop, tcRight, tcBottom])
** DrawImageQuad({imgHandle|nil}, x1, y1, x2, y2, x3, y3, x4, y4[, s1, t1, s2, t2, s3, t3, s4, t4])
** DrawImageQuadAt({imgHandle|nil}, xform, x1, y1, x2, y2, x3, y3, x4, y4[, s1, t1, s2, t2, s3, t3, s4, t4])
** DrawMeshAt({imgHandle|nil}, xform, mshHandle)
** DrawString(left, top, align{"LEFT"|"CENTER"|"RIGHT"|"CENTER_X"|"RIGHT_X"}, height, font{"FIXED"|"VAR"|"VAR BOLD"}, "<text>")
** width = DrawStringWidth(height, font{"FIXED"|"VAR"|"VAR BOLD"}, "<text>")
** index = DrawStringCursorIndex(height, font{"FIXED"|"VAR"|"VAR BOLD"}, "<text>", cursorX, cursorY)
** str = StripEscapes("<string>")
** count = GetAsyncCount()
**
** searchHandle = NewFileSearch("<spec>"[, findDirectories])
** found = searchHandle:NextFile()
** fileName = searchHandle:GetFileName()
** fileSize = searchHandle:GetFileSize()
** modified, date, time = searchHande:GetFileModifiedTime()
**
** provider, version, status = GetCloudProvider(path)
**
** SetWindowTitle("<title>")
** x, y = GetCursorPos()
** SetCursorPos(x, y)
** ShowCursor(doShow)
** down = IsKeyDown("<keyName>")
** Copy("<string>")
** string = Paste()
** compressed = Deflate(uncompressed)
** uncompressed = Inflate(compressed)
** msec = GetTime()
** path = GetScriptPath()
** path = GetRuntimePath()
** path = GetUserPath()
** SetWorkDir("<path>")
** path = GetWorkDir()
** ssID = LaunchSubScript("<scriptText>", "<funcList>", "<subList>"[, ...])
** AbortSubScript(ssID)
** isRunning = IsSubScriptRunning(ssID)
** ... = LoadModule("<modName>"[, ...])
** err, ... = PLoadModule("<modName>"[, ...])
** err, ... = PCall(func[, ...])
** ConPrintf("<format>"[, ...])
** ConPrintTable(table[, noRecurse])
** ConExecute("<cmd>")
** SpawnProcess("<cmdName>"[, "<args>"])
** OpenURL("<url>")
** SetProfiling(isEnabled)
** Restart()
** Exit(["<message>"])
*/

// Grab UI main pointer from the registry
static ui_main_c* GetUIPtr(lua_State* L)
{
	lua_rawgeti(L, LUA_REGISTRYINDEX, 0);
	ui_main_c* ui = (ui_main_c*)lua_touserdata(L, -1);
	lua_pop(L, 1);
	return ui;
}

// =========
// Callbacks
// =========

static int l_SetCallback(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetCallback(name[, func])");
	ui->LAssert(L, lua_isstring(L, 1), "SetCallback() argument 1: expected string, got %s", luaL_typename(L, 1));
	lua_pushvalue(L, 1);
	if (n >= 2) {
		ui->LAssert(L, lua_isfunction(L, 2) || lua_isnil(L, 2), "SetCallback() argument 2: expected function or nil, got %s", luaL_typename(L, 2));
		lua_pushvalue(L, 2);
	}
	else {
		lua_pushnil(L);
	}
	lua_settable(L, lua_upvalueindex(1));
	return 0;
}

static int l_GetCallback(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: GetCallback(name)");
	ui->LAssert(L, lua_isstring(L, 1), "GetCallback() argument 1: expected string, got %s", luaL_typename(L, 1));
	lua_pushvalue(L, 1);
	lua_gettable(L, lua_upvalueindex(1));
	return 1;
}

static int l_SetMainObject(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	lua_pushstring(L, "MainObject");
	if (n >= 1) {
		ui->LAssert(L, lua_istable(L, 1) || lua_isnil(L, 1), "SetMainObject() argument 1: expected table or nil, got %s", luaL_typename(L, 1));
		lua_pushvalue(L, 1);
	}
	else {
		lua_pushnil(L);
	}
	lua_settable(L, lua_upvalueindex(1));
	return 0;
}

// =============
// Image Handles
// =============

struct imgHandle_s {
	r_shaderHnd_c* hnd;
};

static int l_NewImageHandle(lua_State* L)
{
	imgHandle_s* imgHandle = (imgHandle_s*)lua_newuserdata(L, sizeof(imgHandle_s));
	imgHandle->hnd = NULL;
	lua_pushvalue(L, lua_upvalueindex(1));
	lua_setmetatable(L, -2);
	return 1;
}

static imgHandle_s* GetImgHandle(lua_State* L, ui_main_c* ui, const char* method, bool loaded)
{
	ui->LAssert(L, ui->IsUserData(L, 1, "uiimghandlemeta"), "imgHandle:%s() must be used on an image handle", method);
	imgHandle_s* imgHandle = (imgHandle_s*)lua_touserdata(L, 1);
	lua_remove(L, 1);
	if (loaded) {
		ui->LAssert(L, imgHandle->hnd != NULL, "imgHandle:%s(): image handle has no image loaded", method);
	}
	return imgHandle;
}

static int l_imgHandleGC(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "__gc", false);
	delete imgHandle->hnd;
	return 0;
}

static int l_imgHandleLoad(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "Load", false);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: imgHandle:Load(fileName[, flag1[, flag2...]])");
	ui->LAssert(L, lua_isstring(L, 1), "imgHandle:Load() argument 1: expected string, got %s", luaL_typename(L, 1));
	const char* fileName = lua_tostring(L, 1);
	char fullFileName[512];
	if (strchr(fileName, ':') || !ui->scriptWorkDir) {
		strcpy(fullFileName, fileName);
	}
	else {
		sprintf(fullFileName, "%s/%s", ui->scriptWorkDir, fileName);
	}
	delete imgHandle->hnd;
	int flags = TF_NOMIPMAP;
	for (int f = 2; f <= n; f++) {
		if (!lua_isstring(L, f)) {
			continue;
		}
		const char* flag = lua_tostring(L, f);
		if (!strcmp(flag, "ASYNC")) {
			// Async texture loading removed
		}
		else if (!strcmp(flag, "CLAMP")) {
			flags |= TF_CLAMP;
		}
		else if (!strcmp(flag, "MIPMAP")) {
			flags &= ~TF_NOMIPMAP;
		}
		else if (!strcmp(flag, "NEAREST")) {
			flags |= TF_NEAREST;
		}
		else {
			ui->LAssert(L, 0, "imgHandle:Load(): unrecognised flag '%s'", flag);
		}
	}
	imgHandle->hnd = ui->renderer->RegisterShader(fullFileName, flags);
	return 0;
}

static int l_imgHandleUnload(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "Unload", false);
	delete imgHandle->hnd;
	imgHandle->hnd = NULL;
	return 0;
}

static int l_imgHandleIsValid(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "IsValid", false);
	lua_pushboolean(L, imgHandle->hnd != NULL);
	return 1;
}

static int l_imgHandleIsLoading(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "IsLoading", true);
	int width, height;
	ui->renderer->GetShaderImageSize(imgHandle->hnd, width, height);
	lua_pushboolean(L, width == 0);
	return 1;
}

static int l_imgHandleSetLoadingPriority(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "SetLoadingPriority", true);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: imgHandle:SetLoadingPriority(pri)");
	ui->LAssert(L, lua_isnumber(L, 1), "imgHandle:SetLoadingPriority() argument 1: expected number, got %s", luaL_typename(L, 1));
	ui->renderer->SetShaderLoadingPriority(imgHandle->hnd, (int)lua_tointeger(L, 1));
	return 0;
}

static int l_imgHandleImageSize(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	imgHandle_s* imgHandle = GetImgHandle(L, ui, "ImageSize", true);
	int width, height;
	ui->renderer->GetShaderImageSize(imgHandle->hnd, width, height);
	lua_pushinteger(L, width);
	lua_pushinteger(L, height);
	return 2;
}

// ============
// Mesh Handles
// ============

struct meshHandle_s {
	r_meshHnd_t mesh;
};

static int l_NewMeshHandle(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	int const n = lua_gettop(L);
	ui->LAssert(L, n >= 2, "Usage: NewMeshHandle(positions, texcoords[, indices])");
	ui->LAssert(L, lua_istable(L, 1), "NewMeshHandle() argument 1: expected table, got %s", luaL_typename(L, 1));
	ui->LAssert(L, lua_istable(L, 2), "NewMeshHandle() argument 2: expected table, got %s", luaL_typename(L, 2));
	int const positionsLen = (int)lua_objlen(L, 1);
	int const texcoordsLen = (int)lua_objlen(L, 2);
	ui->LAssert(L, positionsLen == texcoordsLen, "NewMeshHandle() argument 1 and 2: expected same element count, got %d and %d", positionsLen, texcoordsLen);
	ui->LAssert(L, (positionsLen % 2) == 0, "NewMeshHandle() argument 1 and 2: expected even number of elements, got %d", positionsLen);
	int const numVertices = positionsLen / 2;

	int numIndices{};
	bool const hasIndices = n >= 3;
	if (hasIndices) {
		ui->LAssert(L, lua_istable(L, 3), "NewMeshHandle() argument 3: expected table, got %s", luaL_typename(L, 3));
		numIndices = (int)lua_objlen(L, 3);
		ui->LAssert(L, (numIndices % 3) == 0, "NewMeshHandle() argument 3, expected element count divisible by 3, got %d", numIndices);
	}
	else {
		numIndices = numVertices;
	}

	// As ui->LAssert alters control flow without unwinding we cannot allocate anything before all invariants have been established.
	// The first pass checks invariants and the second pass fetches data to construct objects.
	r_meshHnd_t mesh{};
	std::unique_ptr<r_meshVtx_s[]> vertices;
	std::unique_ptr<r_meshIdx_t[]> indices;
	for (int pass = 0; pass < 2; ++pass) {
		if (pass == 1) {
			vertices = std::make_unique<r_meshVtx_s[]>(numVertices);
			indices = std::make_unique<r_meshIdx_t[]>(numIndices);
		}

		for (int i = 1; i <= positionsLen; ++i) {
			int const vtxIdx = (i - 1) / 2;
			int const compIdx = (i - 1) % 2;

			// position table is {x1, y1, x2, y2, ..}
			// texcoord table is {s1, t1, s2, t2, ..}

			lua_rawgeti(L, 1, i); // position to -2
			lua_rawgeti(L, 2, i); // texcoord to -1
			if (pass == 0) {
				ui->LAssert(L, lua_isnumber(L, -2), "NewMeshHandle() argument 1[%d]: expected number, got %s", vtxIdx + 1, luaL_typename(L, -2));
				ui->LAssert(L, lua_isnumber(L, -1), "NewMeshHandle() argument 2[%d]: expected number, got %s", vtxIdx + 1, luaL_typename(L, -1));
			}
			else {
				vertices[vtxIdx].position[compIdx] = (float)lua_tonumber(L, -2);
				vertices[vtxIdx].texcoord[compIdx] = (float)lua_tonumber(L, -1);
			}
			lua_pop(L, 2);
		}

		if (hasIndices) {
			for (int i = 1; i <= numIndices; ++i) {
				lua_rawgeti(L, 3, i); // index to -1
				if (pass == 0) {
					ui->LAssert(L, lua_isnumber(L, -1), "NewMeshHandle() argument 3[%d]: expected number, got %s", i, luaL_typename(L, -1));
					double num = lua_tonumber(L, -1);
					auto index = (int)num;
					ui->LAssert(L, num == (double)index, "NewMeshHandle() argument 3[%d]: expected integer, got %f", i, num);
					ui->LAssert(L, index >= 1 && index <= numVertices, "NewMeshHandle() argument 3[%d]: value %d out of bounds for %d vertices", i, index, numVertices);
				}
				else {
					indices[i - 1] = (int)lua_tonumber(L, -1) - 1;
				}
				lua_pop(L, 1);
			}
		}
		else {
			if (pass == 0) {
				ui->LAssert(L, (numVertices % 3) == 0, "NewMeshHandle() argument 1 and 2: vertex count must be divisible by three when no indices are provided");
			}
			else {
				std::iota(indices.get(), indices.get()  + numVertices, 0);
			}
		}

		if (pass == 1) {
			mesh = ui->renderer->NewMeshHandle(numVertices, vertices.get(), numIndices, indices.get());
		}
	}

	meshHandle_s* meshHandle = (meshHandle_s*)lua_newuserdata(L, sizeof(meshHandle_s));
	meshHandle->mesh = mesh;
	lua_pushvalue(L, lua_upvalueindex(1));
	lua_setmetatable(L, -2);

	return 1;
}

static meshHandle_s* GetMeshHandle(lua_State* L, ui_main_c* ui, const char* method)
{
	ui->LAssert(L, ui->IsUserData(L, 1, "uimeshhandlemeta"), "meshHandle:%s() must be used on an mesh handle", method);
	meshHandle_s* meshHandle = (meshHandle_s*)lua_touserdata(L, 1);
	lua_remove(L, 1);
	return meshHandle;
}

static int l_meshHandleGC(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	meshHandle_s* meshHandle = GetMeshHandle(L, ui, "__gc");
	ui->renderer->DeleteMeshHandle(meshHandle->mesh);
	return 0;
}

// =========
// Rendering
// =========

static int l_RenderInit(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->RenderInit();
	return 0;
}

static int l_GetScreenSize(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushinteger(L, ui->renderer->VirtualScreenWidth());
	lua_pushinteger(L, ui->renderer->VirtualScreenHeight());
	return 2;
}

static int l_SetClearColor(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 3, "Usage: SetClearColor(red, green, blue[, alpha])");
	col4_t color;
	for (int i = 1; i <= 3; i++) {
		ui->LAssert(L, lua_isnumber(L, i), "SetClearColor() argument %d: expected number, got %s", i, luaL_typename(L, i));
		color[i - 1] = (float)lua_tonumber(L, i);
	}
	if (n >= 4 && !lua_isnil(L, 4)) {
		ui->LAssert(L, lua_isnumber(L, 4), "SetClearColor() argument 4: expected number or nil, got %s", luaL_typename(L, 4));
		color[3] = (float)lua_tonumber(L, 4);
	}
	else {
		color[3] = 1.0;
	}
	ui->renderer->SetClearColor(color);
	return 0;
}

static int l_SetDrawLayer(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "SetDrawLayer() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetDrawLayer({layer|nil}[, subLayer])");
	ui->LAssert(L, lua_isnumber(L, 1) || lua_isnil(L, 1), "SetDrawLayer() argument 1: expected number or nil, got %s", luaL_typename(L, 1));
	if (n >= 2) {
		ui->LAssert(L, lua_isnumber(L, 2), "SetDrawLayer() argument 2: expected number, got %s", luaL_typename(L, 2));
	}
	if (lua_isnil(L, 1)) {
		ui->LAssert(L, n >= 2, "SetDrawLayer(): must provide subLayer if layer is nil");
		ui->renderer->SetDrawSubLayer((int)lua_tointeger(L, 2));
	}
	else if (n >= 2) {
		ui->renderer->SetDrawLayer((int)lua_tointeger(L, 1), (int)lua_tointeger(L, 2));
	}
	else {
		ui->renderer->SetDrawLayer((int)lua_tointeger(L, 1));
	}
	return 0;
}

static int l_GetDrawLayer(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushinteger(L, ui->renderer->GetDrawLayer());
	return 1;
}

static int l_SetViewport(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "SetViewport() called outside of OnFrame");
	int n = lua_gettop(L);
	if (n) {
		ui->LAssert(L, n >= 4, "Usage: SetViewport([x, y, width, height])");
		for (int i = 1; i <= 4; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "SetViewport() argument %d: expected number, got %s", i, luaL_typename(L, i));
		}
		ui->renderer->SetViewport((int)lua_tointeger(L, 1), (int)lua_tointeger(L, 2), (int)lua_tointeger(L, 3), (int)lua_tointeger(L, 4));
	}
	else {
		ui->renderer->SetViewport();
	}
	return 0;
}

static int l_SetBlendMode(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "SetViewport() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetBlendMode(mode)");
	static const char* modeMap[6] = { "ALPHA", "PREALPHA", "ADDITIVE", NULL };
	ui->renderer->SetBlendMode(luaL_checkoption(L, 1, "ALPHA", modeMap));
	return 0;
}

static int l_SetDrawColor(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "SetDrawColor() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetDrawColor(red, green, blue[, alpha]) or SetDrawColor(escapeStr)");
	col4_t color;
	if (lua_type(L, 1) == LUA_TSTRING) {
		ui->LAssert(L, IsColorEscape(lua_tostring(L, 1)), "SetDrawColor() argument 1: invalid color escape sequence");
		ReadColorEscape(lua_tostring(L, 1), color);
		color[3] = 1.0;
	}
	else {
		ui->LAssert(L, n >= 3, "Usage: SetDrawColor(red, green, blue[, alpha]) or SetDrawColor(escapeStr)");
		for (int i = 1; i <= 3; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "SetDrawColor() argument %d: expected number, got %s", i, luaL_typename(L, i));
			color[i - 1] = (float)lua_tonumber(L, i);
		}
		if (n >= 4 && !lua_isnil(L, 4)) {
			ui->LAssert(L, lua_isnumber(L, 4), "SetDrawColor() argument 4: expected number or nil, got %s", luaL_typename(L, 4));
			color[3] = (float)lua_tonumber(L, 4);
		}
		else {
			color[3] = 1.0;
		}
	}
	ui->renderer->DrawColor(color);
	return 0;
}

static int l_DrawImage(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "DrawImage() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 5, "Usage: DrawImage({imgHandle|nil}, left, top, width, height[, tcLeft, tcTop, tcRight, tcBottom])");
	ui->LAssert(L, lua_isnil(L, 1) || ui->IsUserData(L, 1, "uiimghandlemeta"), "DrawImage() argument 1: expected image handle or nil, got %s", luaL_typename(L, 1));
	r_shaderHnd_c* hnd = NULL;
	if (!lua_isnil(L, 1)) {
		imgHandle_s* imgHandle = (imgHandle_s*)lua_touserdata(L, 1);
		ui->LAssert(L, imgHandle->hnd != NULL, "DrawImage(): image handle has no image loaded");
		hnd = imgHandle->hnd;
	}
	float arg[8];
	if (n > 5) {
		ui->LAssert(L, n >= 9, "DrawImage(): incomplete set of texture coordinates provided");
		for (int i = 2; i <= 9; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImage() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 2] = (float)lua_tonumber(L, i);
		}
		ui->renderer->DrawImage(hnd, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7]);
	}
	else {
		for (int i = 2; i <= 5; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImage() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 2] = (float)lua_tonumber(L, i);
		}
		ui->renderer->DrawImage(hnd, arg[0], arg[1], arg[2], arg[3]);
	}
	return 0;
}

static int l_DrawImageQuad(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "DrawImageQuad() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 9, "Usage: DrawImageQuad({imgHandle|nil}, x1, y1, x2, y2, x3, y3, x4, y4[, s1, t1, s2, t2, s3, t3, s4, t4])");
	ui->LAssert(L, lua_isnil(L, 1) || ui->IsUserData(L, 1, "uiimghandlemeta"), "DrawImageQuad() argument 1: expected image handle or nil, got %s", luaL_typename(L, 1));
	r_shaderHnd_c* hnd = NULL;
	if (!lua_isnil(L, 1)) {
		imgHandle_s* imgHandle = (imgHandle_s*)lua_touserdata(L, 1);
		ui->LAssert(L, imgHandle->hnd != NULL, "DrawImageQuad(): image handle has no image loaded");
		hnd = imgHandle->hnd;
	}
	float arg[16];
	if (n > 9) {
		ui->LAssert(L, n >= 17, "DrawImageQuad(): incomplete set of texture coordinates provided");
		for (int i = 2; i <= 17; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImageQuad() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 2] = (float)lua_tonumber(L, i);
		}
		ui->renderer->DrawImageQuad(hnd, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15]);
	}
	else {
		for (int i = 2; i <= 9; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImageQuad() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 2] = (float)lua_tonumber(L, i);
		}
		ui->renderer->DrawImageQuad(hnd, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7]);
	}
	return 0;
}

static int l_DrawImageAt(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "DrawImageAt() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 6, "Usage: DrawImageAt({imgHandle|nil}, xform, left, top, width, height[, tcLeft, tcTop, tcRight, tcBottom])");
	ui->LAssert(L, lua_isnil(L, 1) || ui->IsUserData(L, 1, "uiimghandlemeta"), "DrawImage() argument 1: expected image handle or nil, got %s", luaL_typename(L, 1));
	r_shaderHnd_c* hnd = NULL;
	if (!lua_isnil(L, 1)) {
		imgHandle_s* imgHandle = (imgHandle_s*)lua_touserdata(L, 1);
		ui->LAssert(L, imgHandle->hnd != NULL, "DrawImageAt(): image handle has no image loaded");
		hnd = imgHandle->hnd;
	}
	glm::mat3x3 xform(1);
	{
		ui->LAssert(L, lua_istable(L, 2), "DrawImageAt() argument 2: expected 2x3 matrix table, got %s", luaL_typename(L, 2));
		ui->LAssert(L, lua_objlen(L, 2) == 6, "DrawImageAt() argument 2: expected 6-element matrix, got %d", lua_objlen(L, 2));
		for (int i = 1; i <= 6; ++i) {
			lua_rawgeti(L, 2, i);
			ui->LAssert(L, lua_isnumber(L, -1), "DrawImageAt() argument 2[%d]: expected number, got %s", i, lua_typename(L, -1));
			int c = (i - 1) % 3;
			int r = (i - 1) / 3;
			xform[c][r] = (float)lua_tonumber(L, -1);
			lua_pop(L, 1);
		}
	}
	float arg[8];
	bool const hasTc = n > 6;
	if (hasTc) {
		ui->LAssert(L, n >= 10, "DrawImageAt(): incomplete set of texture coordinates provided");
		for (int i = 3; i <= 10; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImageAt() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 3] = (float)lua_tonumber(L, i);
		}
	}
	else {
		for (int i = 3; i <= 6; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImageAt() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 3] = (float)lua_tonumber(L, i);
		}
	}
	auto x = arg[0], y = arg[1], w = arg[2], h = arg[3];
	std::array<glm::vec3, 4> v{
		xform * glm::vec3(x, y, 1.0f),
		xform * glm::vec3(x + w, y, 1.0f),
		xform * glm::vec3(x + w, y + h, 1.0f),
		xform * glm::vec3(x, y + h, 1.0f),
	};
	if (hasTc) {
		auto s0 = arg[4], t0 = arg[5], s1 = arg[6], t1 = arg[7];
		ui->renderer->DrawImageQuad(hnd, v[0].x, v[0].y, v[1].x, v[1].y, v[2].x, v[2].y, v[3].x, v[3].y, s0, t0, s1, t0, s1, t1, s0, t1);
	}
	else {
		ui->renderer->DrawImageQuad(hnd, v[0].x, v[0].y, v[1].x, v[1].y, v[2].x, v[2].y, v[3].x, v[3].y);
	}
	return 0;
}

static int l_DrawImageQuadAt(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "DrawImageQuadAt() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 9, "Usage: DrawImageQuadAt({imgHandle|nil}, xform, x1, y1, x2, y2, x3, y3, x4, y4[, s1, t1, s2, t2, s3, t3, s4, t4])");
	ui->LAssert(L, lua_isnil(L, 1) || ui->IsUserData(L, 1, "uiimghandlemeta"), "DrawImageQuadAt() argument 1: expected image handle or nil, got %s", luaL_typename(L, 1));
	r_shaderHnd_c* hnd = NULL;
	if (!lua_isnil(L, 1)) {
		imgHandle_s* imgHandle = (imgHandle_s*)lua_touserdata(L, 1);
		ui->LAssert(L, imgHandle->hnd != NULL, "DrawImageQuadAt(): image handle has no image loaded");
		hnd = imgHandle->hnd;
	}
	glm::mat3x3 xform(1);
	{
		ui->LAssert(L, lua_istable(L, 2), "DrawImageQuadAt() argument 2: expected 2x3 matrix table, got %s", luaL_typename(L, 2));
		ui->LAssert(L, lua_objlen(L, 2) == 6, "DrawImageQuadAt() argument 2: expected 6-element matrix, got %d", lua_objlen(L, 2));
		for (int i = 1; i <= 6; ++i) {
			lua_rawgeti(L, 2, i);
			ui->LAssert(L, lua_isnumber(L, -1), "DrawImageQuadAt() argument 2[%d]: expected number, got %s", i, luaL_typename(L, -1));
			int c = (i - 1) % 3;
			int r = (i - 1) / 3;
			xform[c][r] = (float)lua_tonumber(L, -1);
			lua_pop(L, 1);
		}
	}
	float arg[16];
	bool const hasTc = n > 10;
	if (hasTc) {
		ui->LAssert(L, n >= 18, "DrawImageQuadAt(): incomplete set of texture coordinates provided");
		for (int i = 3; i <= 18; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImageQuadAt() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 3] = (float)lua_tonumber(L, i);
		}
	}
	else {
		for (int i = 3; i <= 10; i++) {
			ui->LAssert(L, lua_isnumber(L, i), "DrawImageQuadAt() argument %d: expected number, got %s", i, luaL_typename(L, i));
			arg[i - 3] = (float)lua_tonumber(L, i);
		}
	}
	std::array<glm::vec3, 4> v;
	for (int i = 0; i < 4; ++i) {
		size_t base = 2 * i;
		v[i] = xform * glm::vec3(arg[base], arg[base + 1], 1.0f);
	}
	if (hasTc) {
		ui->renderer->DrawImageQuad(hnd, v[0].x, v[0].y, v[1].x, v[1].y, v[2].x, v[2].y, v[3].x, v[3].y, arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15]);
	}
	else {
		ui->renderer->DrawImageQuad(hnd, v[0].x, v[0].y, v[1].x, v[1].y, v[2].x, v[2].y, v[3].x, v[3].y);
	}
	return 0;
}

static int l_DrawMeshAt(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "DrawMeshAt() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n == 3, "Usage: DrawMeshAt({imgHandle|nil}, mshHandle, xform)");
	int k = 1;
	ui->LAssert(L, lua_isnil(L, k) || ui->IsUserData(L, k, "uiimghandlemeta"), "DrawMeshAt() argument %d: expected image handle or nil, got %s", k, luaL_typename(L, k));
	r_shaderHnd_c* imgHnd = NULL;
	if (!lua_isnil(L, k)) {
		imgHandle_s* imgHandle = (imgHandle_s*)lua_touserdata(L, k);
		ui->LAssert(L, imgHandle->hnd != NULL, "DrawMeshAt(): image handle has no image loaded");
		imgHnd = imgHandle->hnd;
	}
	++k;

	glm::mat3x2 xform(1);
	{
		ui->LAssert(L, lua_istable(L, k), "DrawMeshAt() argument %d: expected 2x3 matrix table, got %s", k, luaL_typename(L, k));
		ui->LAssert(L, lua_objlen(L, k) == 6, "DrawMeshAt() argument %d: expected 6-element matrix, got %d", k, lua_objlen(L, k));
		for (int i = 1; i <= 6; ++i) {
			lua_rawgeti(L, k, i);
			ui->LAssert(L, lua_isnumber(L, -1), "DrawMeshAt() argument %d[%d]: expected number, got %s", k, i, luaL_typename(L, -1));
			int c = (i - 1) % 3;
			int r = (i - 1) / 3;
			xform[c][r] = (float)lua_tonumber(L, -1);
			lua_pop(L, 1);
		}
	}
	++k;

	ui->LAssert(L, ui->IsUserData(L, k, "uimeshhandlemeta"), "DrawMeshAt() argument %d: expected mesh handle, got %s", k, luaL_typename(L, k));
	meshHandle_s* mesh = NULL;
	mesh = (meshHandle_s*)lua_touserdata(L, k);
	++k;

	ui->renderer->DrawMesh(imgHnd, mesh->mesh, xform);
	return 0;
}

static int l_DrawString(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	ui->LAssert(L, ui->renderEnable, "DrawString() called outside of OnFrame");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 6, "Usage: DrawString(left, top, align, height, font, text)");
	ui->LAssert(L, lua_isnumber(L, 1), "DrawString() argument 1: expected number, got %s", luaL_typename(L, 1));
	ui->LAssert(L, lua_isnumber(L, 2), "DrawString() argument 2: expected number, got %s", luaL_typename(L, 2));
	ui->LAssert(L, lua_isstring(L, 3) || lua_isnil(L, 3), "DrawString() argument 3: expected string or nil, got %s", luaL_typename(L, 3));
	ui->LAssert(L, lua_isnumber(L, 4), "DrawString() argument 4: expected number, got %s", luaL_typename(L, 4));
	ui->LAssert(L, lua_isstring(L, 5), "DrawString() argument 5: expected string, got %s", luaL_typename(L, 5));
	ui->LAssert(L, lua_isstring(L, 6), "DrawString() argument 6: expected string, got %s", luaL_typename(L, 6));
	static const char* alignMap[6] = { "LEFT", "CENTER", "RIGHT", "CENTER_X", "RIGHT_X", NULL };
	static const char* fontMap[4] = { "FIXED", "VAR", "VAR BOLD", NULL };
	ui->renderer->DrawString(
		(float)lua_tonumber(L, 1), (float)lua_tonumber(L, 2), luaL_checkoption(L, 3, "LEFT", alignMap),
		(int)lua_tointeger(L, 4), NULL, luaL_checkoption(L, 5, "FIXED", fontMap), lua_tostring(L, 6)
	);
	return 0;
}

static int l_DrawStringWidth(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 3, "Usage: DrawStringWidth(height, font, text)");
	ui->LAssert(L, lua_isnumber(L, 1), "DrawStringWidth() argument 1: expected number, got %s", luaL_typename(L, 1));
	ui->LAssert(L, lua_isstring(L, 2), "DrawStringWidth() argument 2: expected string, got %s", luaL_typename(L, 2));
	ui->LAssert(L, lua_isstring(L, 3), "DrawStringWidth() argument 3: expected string, got %s", luaL_typename(L, 3));
	static const char* fontMap[4] = { "FIXED", "VAR", "VAR BOLD", NULL };
	lua_pushinteger(L, ui->renderer->DrawStringWidth((int)lua_tointeger(L, 1), luaL_checkoption(L, 2, "FIXED", fontMap), lua_tostring(L, 3)));
	return 1;
}

static int l_DrawStringCursorIndex(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 5, "Usage: DrawStringCursorIndex(height, font, text, cursorX, cursorY)");
	ui->LAssert(L, lua_isnumber(L, 1), "DrawStringCursorIndex() argument 1: expected number, got %s", luaL_typename(L, 1));
	ui->LAssert(L, lua_isstring(L, 2), "DrawStringCursorIndex() argument 2: expected string, got %s", luaL_typename(L, 2));
	ui->LAssert(L, lua_isstring(L, 3), "DrawStringCursorIndex() argument 3: expected string, got %s", luaL_typename(L, 3));
	ui->LAssert(L, lua_isnumber(L, 4), "DrawStringCursorIndex() argument 4: expected number, got %s", luaL_typename(L, 4));
	ui->LAssert(L, lua_isnumber(L, 5), "DrawStringCursorIndex() argument 5: expected number, got %s", luaL_typename(L, 5));
	static const char* fontMap[4] = { "FIXED", "VAR", "VAR BOLD", NULL };
	lua_pushinteger(L, ui->renderer->DrawStringCursorIndex((int)lua_tointeger(L, 1), luaL_checkoption(L, 2, "FIXED", fontMap), lua_tostring(L, 3), (int)lua_tointeger(L, 4), (int)lua_tointeger(L, 5)) + 1);
	return 1;
}

static int l_StripEscapes(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: StripEscapes(string)");
	ui->LAssert(L, lua_isstring(L, 1), "StripEscapes() argument 1: expected string, got %s", luaL_typename(L, 1));
	const char* str = lua_tostring(L, 1);
	char* strip = new char[strlen(str) + 1];
	char* p = strip;
	while (*str) {
		int esclen = IsColorEscape(str);
		if (esclen) {
			str += esclen;
		}
		else {
			*(p++) = *(str++);
		}
	}
	*p = 0;
	lua_pushstring(L, strip);
	delete[] strip;
	return 1;
}

static int l_GetAsyncCount(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->LAssert(L, ui->renderer != NULL, "Renderer is not initialised");
	lua_pushinteger(L, ui->renderer->GetTexAsyncCount());
	return 1;
}

// ==============
// Search Handles
// ==============

struct searchHandle_s {
	find_c* find;
	bool	dirOnly;
};

static int l_NewFileSearch(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: NewFileSearch(spec[, findDirectories])");
	ui->LAssert(L, lua_isstring(L, 1), "NewFileSearch() argument 1: expected string, got %s", luaL_typename(L, 1));
	find_c* find = new find_c();
	if (!find->FindFirst(lua_tostring(L, 1))) {
		delete find;
		return 0;
	}
	bool dirOnly = lua_toboolean(L, 2) != 0;
	while (find->isDirectory != dirOnly || find->fileName == "." || find->fileName == "..") {
		if (!find->FindNext()) {
			delete find;
			return 0;
		}
	}
	searchHandle_s* searchHandle = (searchHandle_s*)lua_newuserdata(L, sizeof(searchHandle_s));
	searchHandle->find = find;
	searchHandle->dirOnly = dirOnly;
	lua_pushvalue(L, lua_upvalueindex(1));
	lua_setmetatable(L, -2);
	return 1;
}

static searchHandle_s* GetSearchHandle(lua_State* L, ui_main_c* ui, const char* method, bool valid)
{
	ui->LAssert(L, ui->IsUserData(L, 1, "uisearchhandlemeta"), "searchHandle:%s() must be used on a search handle", method);
	searchHandle_s* searchHandle = (searchHandle_s*)lua_touserdata(L, 1);
	lua_remove(L, 1);
	if (valid) {
		ui->LAssert(L, searchHandle->find != NULL, "searchHandle:%s(): search handle is no longer valid (ran out of files to find)", method);
	}
	return searchHandle;
}

static int l_searchHandleGC(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	searchHandle_s* searchHandle = GetSearchHandle(L, ui, "__gc", false);
	delete searchHandle->find;
	return 0;
}

static int l_searchHandleNextFile(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	searchHandle_s* searchHandle = GetSearchHandle(L, ui, "NextFile", true);
	do {
		if (!searchHandle->find->FindNext()) {
			delete searchHandle->find;
			searchHandle->find = NULL;
			return 0;
		}
	} while (searchHandle->find->isDirectory != searchHandle->dirOnly || searchHandle->find->fileName == "." || searchHandle->find->fileName == "..");
	lua_pushboolean(L, 1);
	return 1;
}

static int l_searchHandleGetFileName(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	searchHandle_s* searchHandle = GetSearchHandle(L, ui, "GetFileName", true);
	lua_pushstring(L, searchHandle->find->fileName.c_str());
	return 1;
}

static int l_searchHandleGetFileSize(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	searchHandle_s* searchHandle = GetSearchHandle(L, ui, "GetFileSize", true);
	lua_pushinteger(L, (lua_Integer)searchHandle->find->fileSize);
	return 1;
}

static int l_searchHandleGetFileModifiedTime(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	searchHandle_s* searchHandle = GetSearchHandle(L, ui, "GetFileModifiedTime", true);
	lua_pushnumber(L, (double)searchHandle->find->modified);
	return 1;
}

// ===================
// Cloud provider info
// ===================

struct CloudProviderInfo {
	std::string name;
	std::string version;
	uint32_t status;
};

#ifdef _WIN32
#include <Windows.h>
#include <cfapi.h>

static std::string NarrowString(std::wstring_view ws) {
	auto cb = WideCharToMultiByte(CP_UTF8, 0, ws.data(), (int)ws.size(), nullptr, 0, nullptr, nullptr);
	std::string ret(cb, '\0');
	WideCharToMultiByte(CP_UTF8, 0, ws.data(), (int)ws.size(), ret.data(), (int)ret.size(), nullptr, nullptr);
	return ret;
}

struct CloudProviderLibrary {
	CloudProviderLibrary() {
		cldLib = LoadLibraryW(L"cldapi.dll");
		if (cldLib != nullptr) {
			CfGetSyncRootInfoByPath = (decltype (CfGetSyncRootInfoByPath))GetProcAddress(cldLib, "CfGetSyncRootInfoByPath");
		}
	}

	~CloudProviderLibrary() {
		FreeLibrary(cldLib);
	}

	bool Loaded() const { return cldLib != nullptr && CfGetSyncRootInfoByPath != nullptr; }

	CloudProviderLibrary(CloudProviderLibrary const&) = delete;
	CloudProviderLibrary& operator = (CloudProviderLibrary const&) = delete;

	decltype (&::CfGetSyncRootInfoByPath) CfGetSyncRootInfoByPath{};

	HMODULE cldLib{};
};

static std::optional<CloudProviderInfo> GetCloudProviderInfo(std::filesystem::path const& path) {
	HRESULT hr{ S_OK };
	DWORD len{};
	static std::vector<char> buf(65536);
	static CloudProviderLibrary lib;
	if (!lib.Loaded()) {
		return {};
	}
	hr = lib.CfGetSyncRootInfoByPath(path.generic_wstring().c_str(), CF_SYNC_ROOT_INFO_PROVIDER, buf.data(), (DWORD)buf.size(), &len);
	if (FAILED(hr) && GetLastError() != ERROR_MORE_DATA) {
		return {};
	}
	auto* syncRootInfo = (CF_SYNC_ROOT_PROVIDER_INFO const*)buf.data();
	buf.resize(len);
	hr = lib.CfGetSyncRootInfoByPath(path.c_str(), CF_SYNC_ROOT_INFO_PROVIDER, buf.data(), len, &len);
	if (FAILED(hr)) {
		return {};
	}
	CloudProviderInfo ret{};
	ret.name = NarrowString(syncRootInfo->ProviderName);
	ret.version = NarrowString(syncRootInfo->ProviderVersion);
	ret.status = syncRootInfo->ProviderStatus;
	return ret;
}
#else
static std::optional<CloudProviderInfo> GetCloudProviderInfo(std::filesystem::path const& path) {
	return {};
}
#endif

static int l_GetCloudProvider(lua_State* L) {
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: GetCloudProvider(path)");
	ui->LAssert(L, lua_isstring(L, 1), "GetCloudProvider() argument 1: expected string, got %s", luaL_typename(L, 1));

	auto info = GetCloudProviderInfo(lua_tostring(L, 1));
	if (info) {
		lua_pushstring(L, info->name.c_str());
		lua_pushstring(L, info->version.c_str());
		lua_pushinteger(L, info->status);
		return 3;
	}

	return 0;
}

// =================
// General Functions
// =================

static int l_SetWindowTitle(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetWindowTitle(title)");
	ui->LAssert(L, lua_isstring(L, 1), "SetWindowTitle() argument 1: expected string, got %s", luaL_typename(L, 1));
	ui->sys->video->SetTitle(lua_tostring(L, 1));
	ui->sys->conWin->SetTitle(lua_tostring(L, 1));
	return 0;
}

static int l_GetCursorPos(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushinteger(L, ui->renderer->VirtualMap(ui->cursorX));
	lua_pushinteger(L, ui->renderer->VirtualMap(ui->cursorY));
	return 2;
}

static int l_SetCursorPos(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 2, "Usage: SetCursorPos(x, y)");
	ui->LAssert(L, lua_isnumber(L, 1), "SetCursorPos() argument 1: expected number, got %s", luaL_typename(L, 1));
	ui->LAssert(L, lua_isnumber(L, 2), "SetCursorPos() argument 2: expected number, got %s", luaL_typename(L, 2));
	int x = ui->renderer->VirtualUnmap((int)lua_tointeger(L, 1));
	int y = ui->renderer->VirtualUnmap((int)lua_tointeger(L, 2));
	ui->sys->video->SetRelativeCursor(x, y);
	return 0;
}

static int l_ShowCursor(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: ShowCursor(doShow)");
	return 0;
}

static int l_IsKeyDown(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: IsKeyDown(keyName)");
	ui->LAssert(L, lua_isstring(L, 1), "IsKeyDown() argument 1: expected string, got %s", luaL_typename(L, 1));
	size_t len;
	const char* kname = lua_tolstring(L, 1, &len);
	ui->LAssert(L, len >= 1, "IsKeyDown() argument 1: string is empty", 1);
	int key = ui->KeyForName(kname);
	ui->LAssert(L, key, "IsKeyDown(): unrecognised key name");
	lua_pushboolean(L, ui->sys->IsKeyDown(key));
	return 1;
}

static int l_Copy(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: Copy(string)");
	ui->LAssert(L, lua_isstring(L, 1), "Copy() argument 1: expected string, got %s", luaL_typename(L, 1));
	ui->sys->ClipboardCopy(lua_tostring(L, 1));
	return 0;
}

static int l_Paste(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	char* data = ui->sys->ClipboardPaste();
	if (data) {
		lua_pushstring(L, data);
		FreeString(data);
		return 1;
	}
	else {
		return 0;
	}
}

static int l_Deflate(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: Deflate(string)");
	ui->LAssert(L, lua_isstring(L, 1), "Deflate() argument 1: expected string, got %s", luaL_typename(L, 1));
	z_stream_s z;
	z.zalloc = NULL;
	z.zfree = NULL;
	deflateInit(&z, 9);
	size_t inLen;
	byte* in = (byte*)lua_tolstring(L, 1, &inLen);
	int outSz = deflateBound(&z, inLen);
	byte* out = new byte[outSz];
	z.next_in = in;
	z.avail_in = inLen;
	z.next_out = out;
	z.avail_out = outSz;
	int err = deflate(&z, Z_FINISH);
	deflateEnd(&z);
	if (err == Z_STREAM_END) {
		lua_pushlstring(L, (const char*)out, z.total_out);
		return 1;
	}
	else {
		lua_pushnil(L);
		lua_pushstring(L, zError(err));
		return 2;
	}
}

static int l_Inflate(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: Inflate(string)");
	ui->LAssert(L, lua_isstring(L, 1), "Inflate() argument 1: expected string, got %s", luaL_typename(L, 1));
	size_t inLen;
	byte* in = (byte*)lua_tolstring(L, 1, &inLen);
	int outSz = inLen * 4;
	byte* out = new byte[outSz];
	z_stream_s z;
	z.next_in = in;
	z.avail_in = inLen;
	z.zalloc = NULL;
	z.zfree = NULL;
	z.next_out = out;
	z.avail_out = outSz;
	inflateInit(&z);
	int err;
	while ((err = inflate(&z, Z_NO_FLUSH)) == Z_OK) {
		if (z.avail_out == 0) {
			// Output buffer filled, embiggen it
			int newSz = outSz << 1;
			trealloc(out, newSz);
			z.next_out = out + outSz;
			z.avail_out = outSz;
			outSz = newSz;
		}
	}
	inflateEnd(&z);
	if (err == Z_STREAM_END) {
		lua_pushlstring(L, (const char*)out, z.total_out);
		return 1;
	}
	else {
		lua_pushnil(L);
		lua_pushstring(L, zError(err));
		return 2;
	}
}

static int l_GetTime(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushinteger(L, ui->sys->GetTime());
	return 1;
}

static int l_GetScriptPath(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushstring(L, ui->scriptPath);
	return 1;
}

static int l_GetRuntimePath(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushstring(L, ui->sys->basePath.c_str());
	return 1;
}

static int l_GetUserPath(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushstring(L, ui->sys->userPath.c_str());
	return 1;
}

static int l_MakeDir(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: MakeDir(path)");
	ui->LAssert(L, lua_isstring(L, 1), "MakeDir() argument 1: expected string, got %s", luaL_typename(L, 1));
	std::filesystem::path path(lua_tostring(L, 1));
	std::error_code ec;
	if (!create_directory(path, ec)) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(ec.value()));
		return 2;
	}
	else {
		lua_pushboolean(L, true);
		return 1;
	}
}

static int l_RemoveDir(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: l_RemoveDir(path)");
	ui->LAssert(L, lua_isstring(L, 1), "l_RemoveDir() argument 1: expected string, got %s", luaL_typename(L, 1));
	std::filesystem::path path(lua_tostring(L, 1));
	std::error_code ec;
	if (!is_directory(path, ec) || ec || remove(path, ec) || ec) {
		lua_pushnil(L);
		lua_pushstring(L, strerror(ec.value()));
		return 2;
	}
	else {
		lua_pushboolean(L, true);
		return 1;
	}
}

static int l_SetWorkDir(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetWorkDir(path)");
	ui->LAssert(L, lua_isstring(L, 1), "SetWorkDir() argument 1: expected string, got %s", luaL_typename(L, 1));
	const char* newWorkDir = lua_tostring(L, 1);
	if (!ui->sys->SetWorkDir(newWorkDir)) {
		if (ui->scriptWorkDir) {
			FreeString(ui->scriptWorkDir);
		}
		ui->scriptWorkDir = AllocString(newWorkDir);
	}
	return 0;
}

static int l_GetWorkDir(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	lua_pushstring(L, ui->scriptWorkDir);
	return 1;
}

static int l_LaunchSubScript(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 3, "Usage: LaunchSubScript(scriptText, funcList, subList[, ...])");
	for (int i = 1; i <= 3; i++) {
		ui->LAssert(L, lua_isstring(L, i), "LaunchSubScript() argument %d: expected string, got %s", i, luaL_typename(L, i));
	}
	for (int i = 4; i <= n; i++) {
		ui->LAssert(L, lua_isnil(L, i) || lua_isboolean(L, i) || lua_isnumber(L, i) || lua_isstring(L, i),
			"LaunchSubScript() argument %d: only nil, boolean, number and string types can be passed to sub script", i);
	}
	dword slot = -1;
	for (dword i = 0; i < ui->subScriptSize; i++) {
		if (!ui->subScriptList[i]) {
			slot = i;
			break;
		}
	}
	if (slot == -1) {
		slot = ui->subScriptSize;
		ui->subScriptSize <<= 1;
		trealloc(ui->subScriptList, ui->subScriptSize);
		for (dword i = slot; i < ui->subScriptSize; i++) {
			ui->subScriptList[i] = NULL;
		}
	}
	ui->subScriptList[slot] = ui_ISubScript::GetHandle(ui, slot);
	if (ui->subScriptList[slot]->Start()) {
		lua_pushlightuserdata(L, (void*)(uintptr_t)slot);
	}
	else {
		lua_pushnil(L);
	}
	return 1;
}

static int l_AbortSubScript(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: AbortSubScript(ssID)");
	ui->LAssert(L, lua_islightuserdata(L, 1), "AbortSubScript() argument 1: expected subscript ID, got %s", luaL_typename(L, 1));
	dword slot = (dword)(uintptr_t)lua_touserdata(L, 1);
	ui->LAssert(L, slot < ui->subScriptSize && ui->subScriptList[slot], "AbortSubScript() argument 1: invalid subscript ID");
	ui->LAssert(L, ui->subScriptList[slot]->IsRunning(), "AbortSubScript(): subscript isn't running");
	ui_ISubScript::FreeHandle(ui->subScriptList[slot]);
	ui->subScriptList[slot] = NULL;
	return 0;
}

static int l_IsSubScriptRunning(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: IsSubScriptRunning(ssID)");
	ui->LAssert(L, lua_islightuserdata(L, 1), "IsSubScriptRunning() argument 1: expected subscript ID, got %s", luaL_typename(L, 1));
	dword slot = (dword)(uintptr_t)lua_touserdata(L, 1);
	ui->LAssert(L, slot < ui->subScriptSize && ui->subScriptList[slot], "IsSubScriptRunning() argument 1: invalid subscript ID");
	lua_pushboolean(L, ui->subScriptList[slot]->IsRunning());
	return 1;
}

static int l_LoadModule(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: LoadModule(name[, ...])");
	ui->LAssert(L, lua_isstring(L, 1), "LoadModule() argument 1: expected string, got %s", luaL_typename(L, 1));
	const char* modName = lua_tostring(L, 1);
	char fileName[1024];
	strcpy(fileName, modName);
	if (!strchr(fileName, '.')) {
		strcat(fileName, ".lua");
	}
	ui->sys->SetWorkDir(ui->scriptPath);
	int err = luaL_loadfile(L, fileName);
	ui->sys->SetWorkDir(ui->scriptWorkDir);
	ui->LAssert(L, err == 0, "LoadModule() error loading '%s' (%d):\n%s", fileName, err, lua_tostring(L, -1));
	lua_replace(L, 1);	// Replace module name with module main chunk
	lua_call(L, n - 1, LUA_MULTRET);
	return lua_gettop(L);
}

static int l_PLoadModule(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: PLoadModule(name[, ...])");
	ui->LAssert(L, lua_isstring(L, 1), "PLoadModule() argument 1: expected string, got %s", luaL_typename(L, 1));
	const char* modName = lua_tostring(L, 1);
	char* fileName = AllocStringLen(strlen(modName) + 4);
	strcpy(fileName, modName);
	if (!strchr(fileName, '.')) {
		strcat(fileName, ".lua");
	}
	ui->sys->SetWorkDir(ui->scriptPath);
	int err = luaL_loadfile(L, fileName);
	ui->sys->SetWorkDir(ui->scriptWorkDir);
	if (err) {
		return 1;
	}
	FreeString(fileName);
	lua_replace(L, 1);	// Replace module name with module main chunk
	lua_getfield(L, LUA_REGISTRYINDEX, "traceback");
	lua_insert(L, 1); // Insert traceback function at start of stack
	err = lua_pcall(L, n - 1, LUA_MULTRET, 1);
	if (err) {
		return 1;
	}
	lua_pushnil(L);
	lua_replace(L, 1); // Replace traceback function with nil
	return lua_gettop(L);
}

static int l_PCall(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: PCall(func[, ...])");
	ui->LAssert(L, lua_isfunction(L, 1), "PCall() argument 1: expected function, got %s", luaL_typename(L, 1));
	lua_getfield(L, LUA_REGISTRYINDEX, "traceback");
	lua_insert(L, 1); // Insert traceback function at start of stack
	int err = lua_pcall(L, n - 1, LUA_MULTRET, 1);
	if (err) {
		return 1;
	}
	lua_pushnil(L);
	lua_replace(L, 1); // Replace traceback function with nil
	return lua_gettop(L);
}

static int l_ConPrintf(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: ConPrintf(fmt[, ...])");
	ui->LAssert(L, lua_isstring(L, 1), "ConPrintf() argument 1: expected string, got %s", luaL_typename(L, 1));
	lua_pushvalue(L, lua_upvalueindex(1));	// string.format
	lua_insert(L, 1);
	lua_call(L, n, 1);
	ui->LAssert(L, lua_isstring(L, 1), "ConPrintf() error: string.format returned non-string");
	ui->sys->con->Printf("%s\n", lua_tostring(L, 1));
	return 0;
}

static void printTableItter(lua_State* L, IConsole* con, int index, int level, bool recurse)
{
	lua_checkstack(L, 5);
	lua_pushnil(L);
	while (lua_next(L, index)) {
		for (int t = 0; t < level; t++) con->Print("  ");
		// Print key
		if (lua_type(L, -2) == LUA_TSTRING) {
			con->Printf("[\"%s^7\"] = ", lua_tostring(L, -2));
		}
		else {
			lua_pushvalue(L, 2);	// Push tostring function
			lua_pushvalue(L, -3);	// Push key
			lua_call(L, 1, 1);		// Call tostring
			con->Printf("%s = ", lua_tostring(L, -1));
			lua_pop(L, 1);			// Pop result of tostring
		}
		// Print value
		if (lua_type(L, -1) == LUA_TTABLE) {
			bool expand = recurse;
			if (expand) {
				lua_pushvalue(L, -1);	// Push value
				lua_gettable(L, 3);		// Index printed tables list
				expand = lua_toboolean(L, -1) == 0;
				lua_pop(L, 1);			// Pop result of indexing
			}
			if (expand) {
				lua_pushvalue(L, -1);	// Push value
				lua_pushboolean(L, 1);
				lua_settable(L, 3);		// Add to printed tables list
				con->Printf("table: %08x {\n", lua_topointer(L, -1));
				printTableItter(L, con, lua_gettop(L), level + 1, true);
				for (int t = 0; t < level; t++) con->Print("  ");
				con->Print("}\n");
			}
			else {
				con->Printf("table: %08x { ... }\n", lua_topointer(L, -1));
			}
		}
		else if (lua_type(L, -1) == LUA_TSTRING) {
			con->Printf("\"%s\"\n", lua_tostring(L, -1));
		}
		else {
			lua_pushvalue(L, 2);	// Push tostring function
			lua_pushvalue(L, -2);	// Push value
			lua_call(L, 1, 1);		// Call tostring
			con->Printf("%s\n", lua_tostring(L, -1));
			lua_pop(L, 1);			// Pop result of tostring
		}
		lua_pop(L, 1);	// Pop value
	}
}

static int l_ConPrintTable(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: ConPrintTable(tbl[, noRecurse])");
	ui->LAssert(L, lua_istable(L, 1), "ConPrintTable() argument 1: expected table, got %s", luaL_typename(L, 1));
	bool recurse = lua_toboolean(L, 2) == 0;
	lua_settop(L, 1);
	lua_getglobal(L, "tostring");
	lua_newtable(L);		// Printed tables list
	lua_pushvalue(L, 1);	// Push root table
	lua_pushboolean(L, 1);
	lua_settable(L, 3);		// Add root table to printed tables list
	printTableItter(L, ui->sys->con, 1, 0, recurse);
	return 0;
}

static int l_ConExecute(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: ConExecute(cmd)");
	ui->LAssert(L, lua_isstring(L, 1), "ConExecute() argument 1: expected string, got %s", luaL_typename(L, 1));
	ui->sys->con->Execute(lua_tostring(L, 1));
	return 0;
}

static int l_ConClear(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->sys->con->Clear();
	return 0;
}

static int l_print(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	lua_getglobal(L, "tostring");
	for (int i = 1; i <= n; i++) {
		lua_pushvalue(L, -1);	// Push tostring function
		lua_pushvalue(L, i);
		lua_call(L, 1, 1);		// Call tostring
		const char* s = lua_tostring(L, -1);
		ui->LAssert(L, s != NULL, "print() error: tostring returned non-string");
		if (i > 1) ui->sys->con->Print(" ");
		ui->sys->con->Print(s);
		lua_pop(L, 1);			// Pop result of tostring
	}
	ui->sys->con->Print("\n");
	return 0;
}

static int l_SpawnProcess(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SpawnProcess(cmdName[, args])");
	ui->LAssert(L, lua_isstring(L, 1), "SpawnProcess() argument 1: expected string, got %s", luaL_typename(L, 1));
	ui->sys->SpawnProcess(lua_tostring(L, 1), lua_tostring(L, 2));
	return 0;
}

static int l_OpenURL(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: OpenURL(url)");
	ui->LAssert(L, lua_isstring(L, 1), "OpenURL() argument 1: expected string, got %s", luaL_typename(L, 1));
	ui->sys->OpenURL(lua_tostring(L, 1));
	return 0;
}

static int l_SetProfiling(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	ui->LAssert(L, n >= 1, "Usage: SetProfiling(isEnabled)");
	ui->debug->SetProfiling(lua_toboolean(L, 1) == 1);
	return 0;
}

static int l_Restart(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	ui->restartFlag = true;
	return 0;
}

static int l_Exit(lua_State* L)
{
	ui_main_c* ui = GetUIPtr(L);
	int n = lua_gettop(L);
	const char* msg = NULL;
	if (n >= 1 && !lua_isnil(L, 1)) {
		ui->LAssert(L, lua_isstring(L, 1), "Exit() argument 1: expected string or nil, got %s", luaL_typename(L, 1));
		msg = lua_tostring(L, 1);
	}
	ui->sys->Exit(msg);
	ui->didExit = true;
	//	lua_pushstring(L, "dummy");
	//	lua_error(L);
	return 0;
}

// ==============================
// Library and API Initialisation
// ==============================

#define ADDFUNC(n) lua_pushcclosure(L, l_##n, 0);lua_setglobal(L, #n);
#define ADDFUNCCL(n, u) lua_pushcclosure(L, l_##n, u);lua_setglobal(L, #n);

int ui_main_c::InitAPI(lua_State* L)
{
	luaL_openlibs(L);

	// Add "lua/" subdir for non-JIT Lua
	{
		lua_getglobal(L, "package");
		char const* tn = lua_typename(L, -1);
		lua_getfield(L, -1, "path");
		std::string old_path = lua_tostring(L, -1);
		lua_pop(L, 1);
		old_path += ";lua/?.lua";
		lua_pushstring(L, old_path.c_str());
		lua_setfield(L, -2, "path");
		lua_pop(L, 1);
	}

	// Callbacks
	lua_newtable(L);		// Callbacks table
	lua_pushvalue(L, -1);	// Push callbacks table
	ADDFUNCCL(SetCallback, 1);
	lua_pushvalue(L, -1);	// Push callbacks table
	ADDFUNCCL(GetCallback, 1);
	lua_pushvalue(L, -1);	// Push callbacks table
	ADDFUNCCL(SetMainObject, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, "uicallbacks");

	// Image handles
	lua_newtable(L);		// Image handle metatable
	lua_pushvalue(L, -1);	// Push image handle metatable
	ADDFUNCCL(NewImageHandle, 1);
	lua_pushvalue(L, -1);	// Push image handle metatable
	lua_setfield(L, -2, "__index");
	lua_pushcfunction(L, l_imgHandleGC);
	lua_setfield(L, -2, "__gc");
	lua_pushcfunction(L, l_imgHandleLoad);
	lua_setfield(L, -2, "Load");
	lua_pushcfunction(L, l_imgHandleUnload);
	lua_setfield(L, -2, "Unload");
	lua_pushcfunction(L, l_imgHandleIsValid);
	lua_setfield(L, -2, "IsValid");
	lua_pushcfunction(L, l_imgHandleIsLoading);
	lua_setfield(L, -2, "IsLoading");
	lua_pushcfunction(L, l_imgHandleSetLoadingPriority);
	lua_setfield(L, -2, "SetLoadingPriority");
	lua_pushcfunction(L, l_imgHandleImageSize);
	lua_setfield(L, -2, "ImageSize");
	lua_setfield(L, LUA_REGISTRYINDEX, "uiimghandlemeta");

	// Mesh handles
	lua_newtable(L); // Mesh handle metatable
	lua_pushvalue(L, -1); // Push mesh handle metatable
	ADDFUNCCL(NewMeshHandle, 1);
	lua_pushvalue(L, -1); // Push mesh handle metatable
	lua_setfield(L, -2, "__index");
	lua_pushcfunction(L, l_meshHandleGC);
	lua_setfield(L, -2, "__gc");
	lua_setfield(L, LUA_REGISTRYINDEX, "uimeshhandlemeta");

	// Rendering
	ADDFUNC(RenderInit);
	ADDFUNC(GetScreenSize);
	ADDFUNC(SetClearColor);
	ADDFUNC(SetDrawLayer);
	ADDFUNC(GetDrawLayer);
	ADDFUNC(SetViewport);
	ADDFUNC(SetBlendMode);
	ADDFUNC(SetDrawColor);
	ADDFUNC(DrawImage);
	ADDFUNC(DrawImageAt);
	ADDFUNC(DrawImageQuad);
	ADDFUNC(DrawImageQuadAt);
	ADDFUNC(DrawMeshAt);
	ADDFUNC(DrawString);
	ADDFUNC(DrawStringWidth);
	ADDFUNC(DrawStringCursorIndex);
	ADDFUNC(StripEscapes);
	ADDFUNC(GetAsyncCount);
	ADDFUNC(RenderInit);

	// Search handles
	lua_newtable(L);	// Search handle metatable
	lua_pushvalue(L, -1);	// Push search handle metatable
	ADDFUNCCL(NewFileSearch, 1);
	lua_pushvalue(L, -1);	// Push search handle metatable
	lua_setfield(L, -2, "__index");
	lua_pushcfunction(L, l_searchHandleGC);
	lua_setfield(L, -2, "__gc");
	lua_pushcfunction(L, l_searchHandleNextFile);
	lua_setfield(L, -2, "NextFile");
	lua_pushcfunction(L, l_searchHandleGetFileName);
	lua_setfield(L, -2, "GetFileName");
	lua_pushcfunction(L, l_searchHandleGetFileSize);
	lua_setfield(L, -2, "GetFileSize");
	lua_pushcfunction(L, l_searchHandleGetFileModifiedTime);
	lua_setfield(L, -2, "GetFileModifiedTime");
	lua_setfield(L, LUA_REGISTRYINDEX, "uisearchhandlemeta");

	// General function
	ADDFUNC(GetCloudProvider);
	ADDFUNC(SetWindowTitle);
	ADDFUNC(GetCursorPos);
	ADDFUNC(SetCursorPos);
	ADDFUNC(ShowCursor);
	ADDFUNC(IsKeyDown);
	ADDFUNC(Copy);
	ADDFUNC(Paste);
	ADDFUNC(Deflate);
	ADDFUNC(Inflate);
	ADDFUNC(GetTime);
	ADDFUNC(GetScriptPath);
	ADDFUNC(GetRuntimePath);
	ADDFUNC(GetUserPath);
	ADDFUNC(MakeDir);
	ADDFUNC(RemoveDir);
	ADDFUNC(SetWorkDir);
	ADDFUNC(GetWorkDir);
	ADDFUNC(LaunchSubScript);
	ADDFUNC(AbortSubScript);
	ADDFUNC(IsSubScriptRunning);
	ADDFUNC(LoadModule);
	ADDFUNC(PLoadModule);
	ADDFUNC(PCall);
	lua_getglobal(L, "string");
	lua_getfield(L, -1, "format");
	ADDFUNCCL(ConPrintf, 1);
	lua_pop(L, 1);		// Pop 'string' table
	ADDFUNC(ConPrintTable);
	ADDFUNC(ConExecute);
	ADDFUNC(ConClear);
	ADDFUNC(print);
	ADDFUNC(SpawnProcess);
	ADDFUNC(OpenURL);
	ADDFUNC(SetProfiling);
	ADDFUNC(Restart);
	ADDFUNC(Exit);
	lua_getglobal(L, "os");
	lua_pushcfunction(L, l_Exit);
	lua_setfield(L, -2, "exit");
	lua_pop(L, 1);		// Pop 'os' table

	return 0;
}

