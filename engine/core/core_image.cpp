// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Core Image
//

#include "common.h"

#include "core_image.h"
#include "core_compress.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "stb_image_resize.h"

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#include "webp/decode.h"

#include <algorithm>
#include <filesystem>
#include <thread>
#include <vector>

#include <gli/gli.hpp>
#include <gsl/span>

// =========
// Raw Image
// =========

image_c::image_c(BorrowedInterfacePtr<IConsole> conHnd)
	: con(conHnd)
{}

bool image_c::CopyRaw(int type, dword inWidth, dword inHeight, const byte* inDat)
{
	gli::format format = gli::format::FORMAT_UNDEFINED;
	const glm::ivec2 extent{ inWidth, inHeight };
	if (type == IMGTYPE_NONE)
		tex = {};
		
	if (type > IMGTYPE_RGBA)
		return false;

	if (inWidth >= (1 << 15) || inHeight >= (1 << 15))
		return false;

	const int comp = type & 0xF;
	size_t dataSize = extent.x * extent.y * comp;

	switch (comp) {
	case 0:
		tex = {};
		return false;
	case 1:
		format = gli::format::FORMAT_L8_UNORM_PACK8;
		break;
	case 3:
		format = gli::format::FORMAT_RGB8_UNORM_PACK8;
		break;
	case 4:
		format = gli::format::FORMAT_RGBA8_UNORM_PACK8;
		break;
	default:
		return false;
	}
	tex = gli::texture2d_array(format, extent, 1, 1);
	if (tex.size(0) == dataSize)
		memcpy(tex.data(0, 0, 0), inDat, dataSize);
	else
		assert(tex.size(0) == dataSize);
	return true;
}

void image_c::Free()
{
	tex = {};
}

bool image_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	return false; // o_O
}

bool image_c::Save(std::filesystem::path const& fileName) 
{
	return false; // o_O
}

std::unique_ptr<image_c> image_c::LoaderForFile(BorrowedInterfacePtr<IConsole> conHnd, std::filesystem::path const& fileName)
{
	auto nameU8 = fileName.generic_u8string();
	std::ifstream in(fileName, std::ios::binary);
	if (!in) {
		conHnd->Warning(fmt::format("'{}' doesn't exist or cannot be opened", nameU8));
		return NULL;
	}

	// Detect first by extension, as decompressing could be expensive.
	if (fileName.extension() == ".zst") {
		auto inner = fileName.filename();
		inner.replace_extension();
		if (inner.extension() == ".dds")
			return std::make_unique<dds_c>(conHnd);
	}
	if (fileName.extension() == ".dds")
		return std::make_unique<dds_c>(conHnd);
	if (fileName.extension() == ".webp")
		return std::make_unique<webp_c>(conHnd);

	// Attempt to detect image file type from first 4 bytes of file
	std::array<byte, 4> dat;
	if (!ReadTrivial(in, dat)) {
		conHnd->Warning(fmt::format("'{}': cannot read image file (file is corrupt?)", nameU8));
		return NULL;
	}
	if (dat[0] == 0xFF && dat[1] == 0xD8) {
		// JPEG Start Of Image marker
		return std::make_unique<jpeg_c>(conHnd);
	} else if (*(dword*)dat.data() == 0x474E5089) {
		// 0x89 P N G
		return std::make_unique<png_c>(conHnd);
	} else if (*(dword*)dat.data() == 0x38464947) {
		// G I F 8
		return std::make_unique<gif_c>(conHnd);
	} else if (*(dword*)dat.data() == 0x20534444) {
		// D D S 0x20
		return std::make_unique<dds_c>(conHnd);
	} else if (*(dword*)dat.data() == 0x46464952) {
		// R I F F
		return std::make_unique<webp_c>(conHnd);
	} else if ((dat[1] == 0 && (dat[2] == 2 || dat[2] == 3 || dat[2] == 10 || dat[2] == 11)) || (dat[1] == 1 && (dat[2] == 1 || dat[2] == 9))) {
		// Detect all valid image types (whether supported or not)
		return std::make_unique<targa_c>(conHnd);
	}
	conHnd->Warning(fmt::format("'{}': unsupported image file format", nameU8));
	return NULL;
}

// ===========
// Targa Image
// ===========

#pragma pack(push,1)
struct tgaHeader_s {
	byte	idLen;
	byte	colorMapType;
	byte	imgType;
	word	colorMapIndex;
	word	colorMapLen;
	byte	colorMapDepth;
	word	xOrigin, yOrigin;
	word	width, height;
	byte	depth;
	byte	descriptor;
};
#pragma pack(pop)

bool targa_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	// Open the file
	std::ifstream in(fileName, std::ios::binary);
	if (!in) {
		return false;
	}

	const auto nameU8 = fileName.generic_u8string();

	// Read header
	tgaHeader_s hdr;
	if (!ReadTrivial(in, hdr)) {
		con->Warning(fmt::format("TGA '{}': couldn't read header", nameU8));
		return false;
	}
	if (hdr.colorMapType) {
		con->Warning(fmt::format("TGA '{}': color mapped images not supported", nameU8));
		return false;
	}
	in.seekg(hdr.idLen, std::ios::cur);
	if (sizeCallback)
		(*sizeCallback)(hdr.width, hdr.height);

	// Try to match image type
	int ittable[3][3] = {
		 3,  8, IMGTYPE_GRAY,
		 2, 24, IMGTYPE_RGB,
		 2, 32, IMGTYPE_RGBA
	};
	int it_m;
	for (it_m = 0; it_m < 3; it_m++) {
		if (ittable[it_m][0] == (hdr.imgType & 7) && ittable[it_m][1] == hdr.depth) break;
	}
	if (it_m == 3) {
		con->Warning(fmt::format("TGA '{}': unsupported image type (it: {} pd: {})", nameU8, hdr.imgType, hdr.depth));
		return false;
	}

	// Read image
	dword width = hdr.width;
	dword height = hdr.height;
	int comp = hdr.depth >> 3;
	int type = ittable[it_m][2];
	int rowSize = width * comp;
	std::vector<byte> datBuf(height * rowSize);
	byte* dat = datBuf.data();
	bool flipV = !(hdr.descriptor & 0x20);
	if (hdr.imgType & 8) {
		// Decode RLE image
		for (dword row = 0; row < height; row++) {
			int rowBase = (flipV? height - row - 1 : row) * rowSize;
			int x = 0;
			do {
				byte rlehdr;
				ReadTrivial(in, rlehdr);
				int rlen = ((rlehdr & 0x7F) + 1) * comp; 
				if (x + rlen > rowSize) {
					con->Warning(fmt::format("TGA '{}': invalid RLE coding (overlong row)", nameU8));
					return false;
				}
				if (rlehdr & 0x80) {
					std::array<byte, 4> rpk;
					ReadTrivial(in, rpk);
					for (int c = 0; c < rlen; c++, x++) dat[rowBase + x] = rpk[c % comp];
				} else {
					in.read((char*)dat + rowBase + x, rlen);
					x+= rlen;
				}
			} while (x < rowSize);
		}
	} else {
		// Raw image
		if (flipV) {
			for (int row = height - 1; row >= 0; row--) {
				in.read((char*)dat + row * rowSize, rowSize);
			}
		} else {
			in.read((char*)dat, height * rowSize);
		}
	}

	// Byteswap BGR(A) to RGB(A)
	if (comp == 3 || comp == 4) {
		uint8_t* p = dat;
		for (size_t i = 0; i < width * height; ++i, p += comp) {
			std::swap(p[0], p[2]);
		}
	}

	return CopyRaw(type, width, height, dat);
}

bool targa_c::Save(std::filesystem::path const& fileName)
{
	auto format = tex.format();
	if (is_compressed(format) || !is_unsigned(format))
		return false;

	int comp = (int)component_count(format);
	if (comp != 3 && comp != 4)
		return false;

	// Open file
	std::ofstream out(fileName, std::ios::binary);
	if (!out) {
		return false;
	}

	auto extent = tex.extent();
	auto rc = stbi_write_tga_to_func([](void* ctx, void* data, int size) {
		auto out = (std::ofstream*)ctx;
		out->write((const char*)data, size);
		}, &out, extent.x, extent.y, comp, tex.data(0, 0, 0));

	return !!rc;
}

// ==========
// JPEG Image
// ==========

bool jpeg_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	Free();

	// Open the file
	const auto fileData = SlurpFile(fileName);
	if (!fileData) {
		return false;
	}

	auto nameU8 = fileName.generic_u8string();

	int x, y, in_comp;
	if (!stbi_info_from_memory((const stbi_uc*)fileData->data(), (int)fileData->size(), &x, &y, &in_comp)) {
		return false;
	}
	if (in_comp != 1 && in_comp != 3) {
		con->Warning(fmt::format("JPEG '{}': unsupported component count '{}'", nameU8, in_comp));
		return false;
	}
	if (sizeCallback)
		(*sizeCallback)(x, y);

	stbi_uc* data = stbi_load_from_memory((const stbi_uc*)fileData->data(), (int)fileData->size(), &x, &y, &in_comp, in_comp);
	if (!data) {
		return false;
	}

	bool success = CopyRaw(in_comp == 1 ? IMGTYPE_GRAY : IMGTYPE_RGB, x, y, data);
	stbi_image_free(data);

	return success;
}

bool jpeg_c::Save(std::filesystem::path const& fileName)
{
	// JPEG only supports RGB and grayscale images
	auto format = tex.format();
	if (is_compressed(format) || !is_unsigned(format))
		return false;

	int comp = (int)component_count(format);
	if (comp != 1 && comp != 3)
		return false;

	// Open the file
	std::ofstream out(fileName, std::ios::binary);
	if (!out) {
		return false;
	}

	auto extent = tex.extent();
	int rc = stbi_write_jpg_to_func([](void* ctx, void* data, int size) {
		auto out = (std::ofstream*)ctx;
		out->write((const char*)data, size);
	}, &out, extent.x, extent.y, comp, tex.data(0, 0, 0), quality);
	return !!rc;
}

// =========
// PNG Image
// =========

bool png_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	Free();

	// Open file and check signature
	const auto fileData = SlurpFile(fileName);
	if (!fileData) {
		return false;
	}

	int x, y, in_comp;
	if (!stbi_info_from_memory((const stbi_uc*)fileData->data(), (int)fileData->size(), &x, &y, &in_comp)) {
		return false;
	}

	dword width = x;
	dword height = y;
	if (sizeCallback)
		(*sizeCallback)(width, height);

	int comp = (in_comp == 1 || in_comp == 3) ? 3 : 4;
	int type = comp == 3 ? IMGTYPE_RGB : IMGTYPE_RGBA;
	stbi_uc* data = stbi_load_from_memory((const stbi_uc*)fileData->data(), (int)fileData->size(), &x, &y, &in_comp, comp);
	if (!data) {
		return false;
	}

	bool success = CopyRaw(type, width, height, data);
	stbi_image_free(data);

	return success;
}

bool png_c::Save(std::filesystem::path const& fileName)
{
	auto format = tex.format();
	if (is_compressed(format) || !is_unsigned(format))
		return false;

	int comp = (int)component_count(format);
	if (comp != 3 && comp != 4)
		return false;

	// Open file
	std::ofstream out(fileName, std::ios::binary);
	if (!out) {
		return false;
	}

	auto extent = tex.extent();
	auto rc = stbi_write_png_to_func([](void* ctx, void* data, int size) {
		auto out = (std::ofstream*)ctx;
		out->write((const char*)data, size);
	}, &out, extent.x, extent.y, comp, tex.data(0, 0, 0), extent.x * comp);
	
	return !!rc;
}

// =========
// GIF Image
// =========

bool gif_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	// Open file
	const auto fileData = SlurpFile(fileName);
	if (!fileData) {
		return false;
	}

	int x, y, in_comp;
	stbi_uc* data = stbi_load_from_memory((const stbi_uc*)fileData->data(), (int)fileData->size(), &x, &y, &in_comp, 4);
	if (!data || in_comp != 4) {
		stbi_image_free(data);
		return false;
	}
	dword width = x;
	dword height = y;
	if (sizeCallback)
		(*sizeCallback)(width, height);

	bool success = CopyRaw(IMGTYPE_RGBA, width, height, data);
	stbi_image_free(data);

	return success;
}

bool gif_c::Save(std::filesystem::path const& fileName)
{
	// HELL no.
	return false;
}

// =========
// DDS Image
// =========

std::optional<glm::ivec2> TryParseDDSSize(gsl::span<const char>& partialData)
{
	constexpr std::array FOURCC_DDS{'D', 'D', 'S', ' '};
	using DdsHeaderPrefix = std::array<uint32_t, 4>;
	if (partialData.size_bytes() < sizeof(FOURCC_DDS) + sizeof(DdsHeaderPrefix))
		return {};
	if (partialData.subspan(0, 4) != gsl::make_span(FOURCC_DDS))
		return {};

	DdsHeaderPrefix headerPrefix;
	std::memcpy(headerPrefix.data(), partialData.subspan(sizeof(FOURCC_DDS), sizeof(DdsHeaderPrefix)).data(), sizeof(DdsHeaderPrefix));
	enum { SizeSlot, FlagsSlot, HeightSlot, WidthSlot };
	return glm::ivec2{headerPrefix[WidthSlot], headerPrefix[HeightSlot]};
}

bool dds_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	// Open file
	auto fileData = SlurpFile(fileName);
	if (!fileData) {
		return false;
	}

	if (fileName.extension() == ".zst" || fileData->size() >= 4 && *(uint32_t*)fileData->data() == 0xFD2FB528) {
		std::optional<DecompressZstandardChunkCallback> chunkCallback;
		if (sizeCallback) {
			chunkCallback = [&](gsl::span<const char> prefix) -> ChunkCallbackResult {
				if (auto size = TryParseDDSSize(prefix)) {
					(*sizeCallback)(size->x, size->y);
					sizeCallback.reset();
					return ChunkCallbackResult::RemoveCallback;
				}
				return ChunkCallbackResult::ContinueDecoding;
			};
		}
		auto ret = DecompressZstandard(as_bytes(gsl::span(*fileData)), chunkCallback);
		if (!ret.has_value())
			return false;
		fileData->assign(ret->data(), ret->data() + ret->size());
	}

	tex = gli::texture2d_array(gli::load_dds((const char*)fileData->data(), fileData->size()));
	if (sizeCallback)
		(*sizeCallback)(tex.extent().x, tex.extent().y);

	return true;
}

bool dds_c::Save(std::filesystem::path const& fileName)
{
	// Nope.
	return false;
}

// =========
// WEBP Image
// =========

bool webp_c::Load(std::filesystem::path const& fileName, std::optional<size_callback_t> sizeCallback)
{
	// Open file
	const auto fileData = SlurpFile(fileName);
	if (!fileData) {
		return false;
	}

	int width;
	int height;

	bool valid = WebPGetInfo((const uint8_t*)fileData->data(), fileData->size(), &width, &height);
	if (!valid)
		return false;

	if (sizeCallback)
		(*sizeCallback)(width, height);
	auto data = WebPDecodeRGBA((const uint8_t*)fileData->data(), fileData->size(), &width, &height);
	bool success = CopyRaw(IMGTYPE_RGBA, width, height, data);
	WebPFree(data);

	return success;
}

bool webp_c::Save(std::filesystem::path const& fileName)
{
	// Nope.
	return false;
}
