#header-only library

vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO g-truc/gli
    REF 3542f8830178061e0661f5df1e89d36ba7d7b0ab
    SHA512 785eba47f08c033b38ebb5ce61ec7a8fd69faf95478da0f47d5d01dce07b89dbc15c33079b4fe9361b0541dd5194d229b635bed747fc4f099f6ea1fbd292b3ba
    HEAD_REF master
    PATCHES
        ignore-vendored-glm.patch
        pr-197-fix-static_assert.patch
        pr-197-remove-make_vec4-conflict.patch
)

vcpkg_cmake_configure(
    SOURCE_PATH "${SOURCE_PATH}"
    OPTIONS "-DGLI_TEST_ENABLE=OFF"
)

vcpkg_cmake_install()

vcpkg_cmake_config_fixup(CONFIG_PATH lib/cmake/gli)

file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug")
file(REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/lib")
file(REMOVE "${CURRENT_PACKAGES_DIR}/include/gli/CMakeLists.txt")

# Put the license file where vcpkg expects it
# manual.md contains the "licenses" section for the project
file(INSTALL "${SOURCE_PATH}/manual.md" DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}" RENAME copyright)