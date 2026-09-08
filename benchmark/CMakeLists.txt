###
# Performance benchmarks.
#
# These are not correctness tests — they run real Puffin simulations and report
# timings, so they take minutes and are sensitive to machine load.  They are
# therefore off by default and registered with the "benchmark" label so they
# stay out of a normal `ctest` run even when enabled.
#
#   cmake -B build -DENABLE_BENCHMARKS=ON .
#   cmake --build build
#   ctest --test-dir build -L benchmark
#
# or, more usefully, drive them directly and keep the JSON for comparison:
#
#   ./benchmark/run_benchmarks.py --puffin build/puffin/puffin -o after.json
###

find_package(Python3 COMPONENTS Interpreter REQUIRED)

set(PUFFIN_BENCHMARK_REPS 3 CACHE STRING
    "Repetitions per benchmark case (the minimum is reported)")

# A serial build (-DENABLE_PARALLEL=OFF) has no launcher and cannot run the
# multi-rank cases, so only the single-rank one is registered. `--serial`
# tells the runner to invoke the executable directly. Timing 1d_serial in both
# modes is the useful comparison: it is the same physics either way, so the
# difference is what the MPI layer costs on one rank.
if (ENABLE_PARALLEL)
  set(_bench_cases 1d_serial 1d_mpi 3d_single 3d_lattice)
  set(_bench_launcher_args "")
  set(_bench_all_case_args "")
else()
  set(_bench_cases 1d_serial)
  set(_bench_launcher_args --serial)
  # The `benchmarks` target below defaults to every case, which would fail on
  # the multi-rank ones here, so name the runnable subset explicitly.
  set(_bench_all_case_args --case 1d_serial)
endif()

foreach(case IN LISTS _bench_cases)
  add_test(
    NAME benchmark_${case}
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_benchmarks.py
            --puffin $<TARGET_FILE:puffin>
            ${_bench_launcher_args}
            --case ${case}
            --reps ${PUFFIN_BENCHMARK_REPS}
            --label "${GIT_REVISION}"
            --output ${CMAKE_CURRENT_BINARY_DIR}/benchmark_${case}.json
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
  set_tests_properties(benchmark_${case} PROPERTIES
    LABELS benchmark
    TIMEOUT 1800
    RUN_SERIAL TRUE
  )
endforeach()

# Convenience target: run every case in one go and write a single JSON.
add_custom_target(benchmarks
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_benchmarks.py
          --puffin $<TARGET_FILE:puffin>
          ${_bench_launcher_args}
          ${_bench_all_case_args}
          --reps ${PUFFIN_BENCHMARK_REPS}
          --label "${GIT_REVISION}"
          --output ${CMAKE_CURRENT_BINARY_DIR}/benchmarks.json
  DEPENDS puffin
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Running Puffin performance benchmarks (this takes several minutes)"
  VERBATIM
)
