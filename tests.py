#!/bin/python

import subprocess
import sys
import argparse
import os
import re

# Initialize parser
parser = argparse.ArgumentParser()
parser.add_argument("-n", "--nasm", action='store_const', const='--nasm-backend', dest='backend')
parser.add_argument("-x", "--x64", action='store_const', const='--x64-backend', dest='backend')

opts = parser.parse_args()

if opts.backend is None:
    opts.backend = '--x64-backend'

failed = False

def read_file(filename):
    f = open(filename, 'r')
    output = f.read()
    f.close()
    return output

def mark_as_failed():
    global failed
    failed = True
    sys.exit(1)

def compile(test_source_file_name):
    print('Compiling %s' % test_source_file_name)
    args = ['./compiler', opts.backend, test_source_file_name]
    print(args)
    compilation = subprocess.run(args, capture_output=True)
    return compilation

def compile_and_run(test_source_file_name, exit_status, expected_output):
    global failed
    compilation = compile(test_source_file_name)
    if compilation.returncode != 0:
        print("Compilation of %s exited with status %s " % (test_source_file_name, compilation.returncode))
        print(str(compilation.stdout, 'utf-8'))
        print(str(compilation.stderr, 'utf-8'))
        mark_as_failed()
        return
    print('Running   %s' % test_source_file_name)
    running = subprocess.run(['./out'], capture_output=True)
    out = str(running.stdout, 'utf-8')
    err = str(running.stderr, 'utf-8')
    if running.returncode != exit_status:
        print("Running of %s exited with status %s not %s as expected " % (test_source_file_name, running.returncode, exit_status))
        print(out)
        print(err)
        mark_as_failed()
        return

    if out != expected_output:
        print("Running of %s exited with unexpected output" % test_source_file_name)
        print('------------------------------------------------------------')
        print('Got')
        print('------------------------------------------------------------')
        print(out)
        print('------------------------------------------------------------')
        print('Wanted')
        print('------------------------------------------------------------')
        print(expected_output)
        mark_as_failed()
        return

def compilation_should_fail(test_source_file_name, exit_status, output_substring):
    global failed
    compilation = compile(test_source_file_name)

    if compilation.returncode != exit_status:
        print("Compilation of %s exited with status %s but expected %s " % (test_source_file_name, compilation.returncode, exit_status))
        print(str(compilation.stdout, 'utf-8'))
        print(str(compilation.stderr, 'utf-8'))
        mark_as_failed()
        return

    if str(compilation.stdout).find(output_substring) == -1:
        print("Did not found expected substring in compilation output of %s" % test_source_file_name)
        print('------------------------------------------------------------')
        print('Got')
        print('------------------------------------------------------------')
        print(str(compilation.stdout, 'utf-8'))
        print('------------------------------------------------------------')
        print('Wanted')
        print('------------------------------------------------------------')
        print(output_substring)
        mark_as_failed()
        return

def compile_and_run_stacktrace_test(test_source_file_name, exit_status, expected_output):
    global failed
    compilation = compile(test_source_file_name)
    if compilation.returncode != 0:
        print("Compilation of %s exited with status %s " % (test_source_file_name, compilation.returncode))
        print(str(compilation.stdout, 'utf-8'))
        print(str(compilation.stderr, 'utf-8'))
        mark_as_failed()
        return

    print('Running   %s' % test_source_file_name)
    running = subprocess.run(['./out'], capture_output=True)
    out = str(running.stdout, 'utf-8')
    err = str(running.stderr, 'utf-8')
    if running.returncode != exit_status:
        print("Running of %s exited with status %s not %s as expected " % (test_source_file_name, running.returncode, exit_status))
        print(out)
        print(err)
        mark_as_failed()
        return

    cwd = os.getcwd() + '/'
    out = re.sub(r'0x[a-f0-9]+ ', '', out) # remove return addresses as they are not deterministic
    out = re.sub(cwd, '', out) # remove workind directory path from stacktrace to make test portable

    if out != expected_output:
        print("Running of %s exited with unexpected output" % test_source_file_name)
        print('------------------------------------------------------------')
        print('Got')
        print('------------------------------------------------------------')
        print(out)
        print('------------------------------------------------------------')
        print('Wanted')
        print('------------------------------------------------------------')
        print(expected_output)
        mark_as_failed()
        return


compile_and_run('tests/000_empty_program.c7', 0, '')
compile_and_run('tests/001_exit_status_1.c7', 1, '')
compile_and_run('tests/002_exit_status_as_sum.c7', 3, '')
compile_and_run('tests/003_function_call.c7', 4, '')
compile_and_run('tests/004_function_call_arguments.c7', 5, '')
compile_and_run('tests/005_hello_world.c7', 11, 'Hello world')
compile_and_run('tests/006_struct_literals.c7', 11, 'Hello world')
compile_and_run('tests/007_if_statement_true.c7', 2, '')
compile_and_run('tests/008_if_statement_false.c7', 3, '')
compile_and_run('tests/009_if_statement_without_else_block_true.c7', 0, 'Hello world')
compile_and_run('tests/010_if_statement_without_else_block_false.c7', 0, '')
compile_and_run('tests/011_signed_int_less_true.c7', 1, '')
compile_and_run('tests/012_signed_int_less_false.c7', 0, '')

# should segfault due to stack overflow
compile_and_run('tests/013_recursion_no_tail_call.c7', -11, '')

# should not segfault: ((1 + 999999)/2*999999) % 256 = 224
compile_and_run('tests/014_recursion_tail_call.c7', 224, '')

compile_and_run('tests/015_file_load/first.c7', 11, 'Hello world')
compile_and_run('tests/016_double_file_load/first.c7', 11, 'Hello world')
compile_and_run('tests/017_circular_file_load/first.c7', 11, 'Hello world')

compilation_should_fail('tests/018_string_as_string_parameter.c7', 1, 'Does not refer to a value')

compile_and_run('tests/019_pattern_matching_simple_assignment.c7', 3, '')
compile_and_run('tests/020_pattern_matching_same_ident_multiple_times.c7', 2, '')

compile_and_run('tests/023_string_as_struct.c7', 11, 'Hello world')

compile_and_run('tests/024_integer_to_string.c7', 6, '123456')

compile_and_run('tests/025_string_escaping.c7', 15, 'H\te\tl\tl\to\nWorld')
compile_and_run('tests/026_print_integer.c7', 0, '123456\n')
compile_and_run('tests/027_arithmetic_operations_on_integers.c7', 0, read_file('tests/027_arithmetic_operations_on_integers.c7.output.txt'))
compile_and_run('tests/028_print_floating_point_number.c7', 0, '0.011719')
compile_and_run('tests/029_arithmetic_operations_on_floats.c7', 0, read_file('tests/029_arithmetic_operations_on_floats.c7.output.txt'))
compile_and_run('tests/030_bug_load_directive_statement_boundary.c7', 0, '')

compilation_should_fail('tests/031_string_should_not_be_allowed_in_syscall.c7', 1, 'Syscall argument should be an i64')

compile_and_run('tests/032_cast_float_to_int_and_back.c7', 0, '1\n2.000000\n')
compile_and_run('tests/033_bool_keywords.c7', 0, 'true branch was taken as it should\nfalse branch was taken as it should\n')
compile_and_run('tests/034_if_branches_can_have_different_types_if_value_is_not_used.c7', 0, 'It works\n')
compile_and_run('tests/035_struct_literal_syntax_ability_to_copy_and_update_existing_struct.c7', 0, 'Hello')
compile_and_run('tests/036_ability_to_skip_explicit_type_if_providing_existing_value_in_struct_literal.c7', 0, 'Hell')
compile_and_run('tests/037_structs_with_non_integral_fields.c7', 0, '1\nAnother\n7\n')
compile_and_run('tests/038_support_library_c_call.c7', 0, '3\n8.000000\n')
compile_and_run('tests/039_floating_point_to_string.c7', 0, '3.141593\n')
compilation_should_fail('tests/040_bug_unknown_identifier_in_pattern_matching.c7', 1, 'Unknown identifier')
compile_and_run('tests/041_bugfeature_variable_changes_type_mid_function.c7', 0, 'It is a string\n3.140000\n')
compile_and_run('tests/042_compile_time_type_enforcing_in_pattern_matching.c7', 0, 'Hello world\n')
compile_and_run('tests/044_short_circuiting.c7', 0, read_file('tests/044_short_circuiting.c7.output.txt'))
compile_and_run('tests/045_bug_statement_boundary_parsing.c7', 0, '')
compile_and_run('tests/047_pattern_matching_simple_struct_destructuring.c7', 11, '')
compilation_should_fail('tests/048_struct_destructuring_field_existence_checking.c7', 1, 'Field was not found at struct')
compilation_should_fail('tests/049_struct_destructuring_struct_type_enforcing.c7', 1, 'Pattern matching type mismatch')
compilation_should_fail('tests/050_struct_destructuring_field_type_enforcing.c7', 1, 'type does not match')
compile_and_run('tests/051_parsing_error_binary_expression_spills_through_statement_boundary.c7', 11, '')
compile_and_run('tests/052_destructuring_without_explicit_type_but_value_was_already_present_in_scope.c7', 11, '')
compile_and_run('tests/053_destructuring_with_explicit_type_but_value_was_already_present_in_scope.c7', 11, '')
compile_and_run('tests/054_destructuring_under_different_name.c7', 11, '')
compile_and_run('tests/055_nested_struct_destructuring.c7', 11, '')
compile_and_run('tests/056_tuple_literals.c7', 1, 'Hello world\n')
compile_and_run('tests/057_tuple_types.c7', 1, 'Hello world\n')
compile_and_run('tests/058_tuple_destructuring.c7', 1, 'Hello world\n')
compile_and_run('tests/059_nested_tuple_destructuring.c7', 2, 'Hello world\n')
compilation_should_fail('tests/060_tuple_destructuring_type_enforcing.c7', 1, 'Explicit type does not match')
compile_and_run('tests/061_tuple_destructuring_type_enforcing_positive.c7', 2, 'Hello world\n')
compilation_should_fail('tests/062_tuple_literal_type_enforcing_negative.c7', 1, 'Explicit type does not match')
compile_and_run('tests/063_tuple_literal_type_enforcing_positive.c7', 5, '')
compile_and_run('tests/064_void_case_statement.c7', 0, '256\nHello world')
compile_and_run('tests/065_unary_ops.c7', 0, '5.000000\n-6.000000\n7\n-8\nfalse\ntrue\n')
compile_and_run('tests/066_string_ops.c7', 0, read_file('tests/066_string_ops.c7.output.txt'))
compile_and_run('tests/067_if_with_different_branch_types.c7', 0, '123\n123_but_string')
compile_and_run('tests/068_atoms.c7', 0, ':my_atom\n:my_other_atom\n')
compile_and_run('tests/069_better_case_statement.c7', 0, '123\n456_but_string\n')
compilation_should_fail('tests/070_better_case_statement_negative.c7', 1, 'Expected to be an identifier')
compilation_should_fail('tests/071_better_case_statement_negative_2.c7', 1, 'Does not refer to a type')
compile_and_run('tests/072_implicit_promotions_to_union.c7', 0, 'Hello\nHello2\n123\n124\n125\n')
compile_and_run('tests/073_function_as_value.c7', 123, '')
compile_and_run('tests/074_return_function_from_function.c7', 124, '')
compile_and_run('tests/075_closures.c7', 4, '')
compile_and_run('tests/076_closures_2.c7', 0, '256\n')
compile_and_run('tests/077_closures_with_non_primitive_types.c7', 0, 'Hello world\n')
compile_and_run('tests/078_closure_with_mixed_types.c7', 0, '123\nHello world\n')
compile_and_run('tests/079_nested_closure_calls.c7', 0, '123\n456\n789\n456\n123\n')
compile_and_run('tests/080_value_capturing_through_two_implicit_closures.c7', 123, '')
compile_and_run('tests/081_function_type.c7', 124, '')
compile_and_run('tests/082_polymorphic_functions.c7', 0, '8\n9.000000\n')
compilation_should_fail('tests/083_poly_function_error_reporting.c7', 1, "Wanted 'f64' and was given 'bool'")
compilation_should_fail('tests/084_poly_function_but_arg_types_are_in_reverse_order.c7', 1, "Wanted 'bool' and was given 'f64'")
compilation_should_fail('tests/085_poly_function_dollar_type_used_multiple_times.c7', 1, "Duplicate polymorphic type declaration")
compilation_should_fail('tests/086_poly_function_tuple_traversing.c7', 1, "Wanted '{f64, string}' and was given '{f64, bool}'")
compilation_should_fail('tests/087_poly_error_message_tuples_with_different_size.c7', 1, "Wanted '{$T, string}' and was given '{f64}'")
compilation_should_fail('tests/088_poly_function_error_message_on_different_arg_count.c7', 1, "Function arguments count mismatch")
compile_and_run('tests/089_poly_function_with_function_as_arg.c7', 0, '101\n')
compile_and_run('tests/090_poly_function_with_union_as_arg.c7', 0, ':nil\nEh?\n')
compile_and_run('tests/091_poly_function_promotion_to_union.c7', 0, 'Eh?\n')
compile_and_run('tests/092_mod_on_negative_integer.c7', 0, '-10 % 100 == -10\n')
compile_and_run('tests/093_list_construction.c7', 1, '')
compile_and_run('tests/094_list_traversing.c7', 0, '1\n2\n3\n')
compile_and_run('tests/095_list_type.c7', 3, '')
compile_and_run('tests/096_list_as_poly_type.c7', 4, '')
compilation_should_fail('tests/097_list_literal_explicit_type.c7', 1, "Wanted 'f64' and was given 'i64'")
compile_and_run('tests/098_list_prepend.c7', 1, '')
compile_and_run('tests/099_list_pattern_matching_several_values_at_once.c7', 3, '')
compile_and_run('tests/100_list_pattern_matching_several_values_at_once_with_rest.c7', 4, '')
compile_and_run('tests/101_list_with_non_primitive_types.c7', 0, 'Hello\nworld\n')
compile_and_run('tests/102_empty_list.c7', 21, '')
compile_and_run('tests/103_lists_foreach.c7', 0, 'List\nof\nstrings\n')
compile_and_run_stacktrace_test('./tests/104_stacktrace.c7', 0, """./tests/104_stacktrace.c7:4:15 a3
./tests/104_stacktrace.c7:10:3 a2
./tests/104_stacktrace.c7:14:3 a1
./tests/104_stacktrace.c7:18:3 main
<unknown>
""")
compile_and_run_stacktrace_test('./tests/105_stacktrace_for_calls_via_function_pointer.c7', 0, """./tests/105_stacktrace_for_calls_via_function_pointer.c7:9:17 <anonymous function>
./tests/105_stacktrace_for_calls_via_function_pointer.c7:4:3 a1
./tests/105_stacktrace_for_calls_via_function_pointer.c7:13:3 main
<unknown>
""")

if failed:
    sys.exit(1)

