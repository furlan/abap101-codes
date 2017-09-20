*&---------------------------------------------------------------------*
*& Report zscr_abapunit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_abapunit.

INTERFACE executor.
  METHODS execute IMPORTING x             TYPE i
                            y             TYPE i
                  RETURNING VALUE(result) TYPE zcalc_result
                  RAISING   zcx_bad_execution.
ENDINTERFACE.

CLASS divider DEFINITION.
  PUBLIC SECTION.
    INTERFACES executor.
ENDCLASS.

CLASS divider IMPLEMENTATION.
  METHOD executor~execute.
    TRY.
        result = x / y.
      CATCH cx_sy_zerodivide INTO DATA(my_bad_division).
        RAISE EXCEPTION TYPE zcx_bad_execution
          EXPORTING
            previous = my_bad_division
            textid   = zcx_bad_execution=>zero_division.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS test_divider DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA test_divider TYPE REF TO divider.
    METHODS setup.
    METHODS test_integer_division FOR TESTING.
    METHODS test_non_integer_division FOR TESTING.
    METHODS test_division_by_zero FOR TESTING.
ENDCLASS.

CLASS test_divider IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->test_divider.
  ENDMETHOD.

  METHOD test_integer_division.
    DATA(actual) = me->test_divider->executor~execute(  x = 4 y = 2 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = actual ).
  ENDMETHOD.

  METHOD test_non_integer_division.
    DATA(actual) = me->test_divider->executor~execute(  x = 10 y = 3 ).
    cl_abap_unit_assert=>assert_equals( exp = '3.33' act = actual ).
  ENDMETHOD.

  METHOD test_division_by_zero.
    TRY.
        DATA(actual) = me->test_divider->executor~execute(  x = 4 y = 0 ).
      CATCH zcx_bad_execution.
        cl_abap_unit_assert=>assert_initial( act = actual ).
      CATCH cx_root.
        cl_abap_unit_assert=>fail( msg = 'Unknown exception.' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS calculator DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING expression TYPE string.
    METHODS calculate RETURNING VALUE(result) TYPE zcalc_result
                      RAISING   zcx_bad_calculation
                                zcx_invalid_expression.

  PRIVATE SECTION.
    DATA expression TYPE string.

ENDCLASS.

CLASS calculator IMPLEMENTATION.

  METHOD constructor.
    me->expression = expression.
  ENDMETHOD.

  METHOD calculate.

    SPLIT me->expression AT space INTO TABLE DATA(tokens).
    DELETE tokens WHERE table_line = space.

    DATA expression_x TYPE i.
    DATA expression_y TYPE i.

    expression_x = tokens[ 1 ].
    DATA(operator) = tokens[ 2 ].
    expression_y = tokens[ 3 ].

    DATA execution TYPE REF TO executor.

    IF operator EQ '/'.
      CREATE OBJECT execution TYPE divider.
      TRY.
          result = execution->execute( x = expression_x y = expression_y ).
        CATCH zcx_bad_execution INTO DATA(my_bad_execution).
          RAISE EXCEPTION TYPE zcx_bad_calculation
            EXPORTING
              previous = my_bad_execution
              textid   = zcx_bad_calculation=>bad_execution.
      ENDTRY.
    ELSE.
      RAISE EXCEPTION TYPE zcx_invalid_expression
        EXPORTING
          textid = zcx_invalid_expression=>invalid_operation.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS test_calculator DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA test_calculator TYPE REF TO calculator.
    METHODS test_invalid_operation FOR TESTING.
    METHODS test_division FOR TESTING.
ENDCLASS.

CLASS test_calculator IMPLEMENTATION.
  METHOD test_invalid_operation.
    CREATE OBJECT me->test_calculator EXPORTING expression = '4 $ 2'.
    TRY.
        me->test_calculator->calculate( ).
        cl_abap_unit_assert=>fail( msg = 'Expected proper exception.' ).
      CATCH zcx_invalid_expression.
        "do nothing.
      CATCH cx_root.
        cl_abap_unit_assert=>fail( msg = 'Unknown exception.' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_division.
    CREATE OBJECT me->test_calculator EXPORTING expression = '4 / 0'.
    TRY.
        me->test_calculator->calculate( ).
        cl_abap_unit_assert=>fail( msg = 'Expected proper exception.' ).
      CATCH zcx_bad_calculation.
        " do nothing.
      CATCH cx_root.
        cl_abap_unit_assert=>fail( msg = 'Unknown exception.' ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

DATA my_calculator TYPE REF TO calculator.

PARAMETERS expr TYPE string.

START-OF-SELECTION.

  CREATE OBJECT my_calculator EXPORTING expression = expr.

  TRY.
      WRITE: / 'Result: ', my_calculator->calculate( ).
    CATCH zcx_bad_calculation INTO DATA(my_bad_calculation).
      WRITE / 'Error division by zero.'.
      DATA previous_exception TYPE REF TO cx_root.
      previous_exception = my_bad_calculation.

      DO 10 TIMES.
        DATA(exception_class) = cl_abap_classdescr=>get_class_name( previous_exception ).
        WRITE: / 'Error on: ', exception_class, previous_exception->get_text( ).
        IF previous_exception->previous IS BOUND.
          previous_exception = previous_exception->previous.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    CATCH zcx_invalid_expression INTO DATA(my_invalid_expression).
      WRITE / my_invalid_expression->get_text( ).
  ENDTRY.
