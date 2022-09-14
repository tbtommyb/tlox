include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src)

find_package(Check REQUIRED)
include_directories(../include)
include_directories(${CHECK_INCLUDE_DIRS})

link_directories(${CHECK_LIBRARY_DIRS})

set(TEST_SOURCES
  check_cloc.c
)

add_executable(check_cloc ${TEST_SOURCES})
target_link_libraries(check_cloc cloc ${CHECK_LIBRARIES})
