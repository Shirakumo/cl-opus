cmake_minimum_required(VERSION 3.1)

project(libopus C)

set(OP_DISABLE_EXAMPLES ON)
set(OP_DISABLE_DOCS ON)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -g")

if(LINUX)
  set(OS_SHORT lin)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -static-libgcc -include ${PROJECT_SOURCE_DIR}/glibc-2.13.h")
elseif(APPLE)
  set(OS_SHORT mac)
elseif(WIN32) 
  set(OS_SHORT win)
else()
  set(OS_SHORT misc)
endif()

if(CMAKE_SYSTEM_PROCESSOR STREQUAL x86_64)
  set(ARCH_SHORT amd64)
else()
  set(ARCH_SHORT CMAKE_SYSTEM_PROCESSOR)
endif()

add_subdirectory(ogg EXCLUDE_FROM_ALL)
add_subdirectory(opus EXCLUDE_FROM_ALL)
add_subdirectory(opusfile EXCLUDE_FROM_ALL)

set_property(TARGET ogg PROPERTY POSITION_INDEPENDENT_CODE ON)
set_property(TARGET opus PROPERTY POSITION_INDEPENDENT_CODE ON)
set_property(TARGET opusfile PROPERTY POSITION_INDEPENDENT_CODE ON)

add_library(miniopus SHARED $<TARGET_OBJECTS:ogg> $<TARGET_OBJECTS:opus> $<TARGET_OBJECTS:opusfile>)
set_target_properties(miniopus PROPERTIES OUTPUT_NAME opus-${OS_SHORT}-${ARCH_SHORT})
target_link_libraries(miniopus m)
set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/../)
install(TARGETS miniopus DESTINATION static)
