#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <vector>

#include <gsl/span>
#include <zstd.h>

std::optional<std::vector<char>> CompressZstandard(gsl::span<const std::byte> src, std::optional<int> level = {});

enum class ChunkCallbackResult
{
	ContinueDecoding,
	AbortDecoding,
	RemoveCallback,
};
using DecompressZstandardChunkCallback = std::function<ChunkCallbackResult(gsl::span<const char>)>;
std::optional<std::vector<char>> DecompressZstandard(gsl::span<const std::byte> src, std::optional<DecompressZstandardChunkCallback> chunkCallback = {});
