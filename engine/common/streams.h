// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Streams Header
//

#include <filesystem>
#include <fstream>
#include <optional>
#include <vector>

inline std::optional<std::vector<char>> SlurpFile(std::ifstream& is, size_t endPadBytes = 0)
{
	if (!is)
		return std::nullopt;

	is.seekg(0, std::ios::end);
	const auto endPos = is.tellg();
	is.seekg(0, std::ios::beg);
	std::vector<char> ret;
	ret.resize((size_t)endPos + endPadBytes);
	is.read(ret.data(), (size_t)endPos);
	if (!is)
		return std::nullopt;
	return ret;
}

inline std::optional<std::vector<char>> SlurpFile(const std::filesystem::path& path, size_t endPadBytes = 0)
{
	std::ifstream is(path, std::ios::binary);
	return SlurpFile(is, endPadBytes);
}

inline size_t GetStreamSize(std::ifstream& s)
{
	const auto prevPos = s.tellg();
	s.seekg(0, std::ios::end);
	const auto endPos = s.tellg();
	s.seekg(prevPos);
	return endPos;
}

template <typename T, size_t N>
std::ifstream& ReadTrivialArray(std::ifstream& is, T(&out)[N])
{
	static_assert(std::is_trivially_copy_assignable_v<T>);
	constexpr auto N = sizeof(T);
	is.read(reinterpret_cast<char*>(&out), N);
	return is;
}

template <typename T>
std::ifstream& ReadTrivial(std::ifstream& is, T& out)
{
	static_assert(std::is_trivially_copy_assignable_v<T>);
	constexpr auto N = sizeof(T);
	is.read(reinterpret_cast<char*>(&out), N);
	return is;
}
