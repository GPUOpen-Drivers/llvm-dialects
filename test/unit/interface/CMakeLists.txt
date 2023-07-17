add_dialects_unit_test(DialectsADTTests
    OpSetTests.cpp
    OpMapTests.cpp
    OpMapIRTests.cpp)

add_dependencies(DialectsADTTests TestDialectTableGen)
