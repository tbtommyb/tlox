set(LIB_SOURCES
  ast.c
  cfg.c
  chunk.c
  codegen.c
  compiler.c
  debug.c
  memory.c
  linked_list.c
  lox_array.c
  object.c
  scanner.c
  table.c
  value.c
  vm.c
)

set(HEADERS
  ${CONFIG_HEADER}
  ../include/cloc.h
)

include_directories(../include)

add_library(cloc STATIC ${LIB_SOURCES} ${HEADERS})

install(TARGETS cloc
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}../include/cloc.h DESTINATION include)
