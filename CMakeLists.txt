# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

cmake_minimum_required(VERSION 3.15)

include(FetchContent)

FetchContent_Declare(
  trieste
  GIT_REPOSITORY https://github.com/microsoft/trieste
  GIT_TAG        main
)

FetchContent_MakeAvailable(trieste)

project(infix_lang)

set (SOURCES src/main.cc src/parser.cc src/evaluate.cc)

add_executable(infixlang ${SOURCES})

target_link_libraries(infixlang PRIVATE trieste)

target_include_directories(infixlang PRIVATE src)
