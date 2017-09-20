*&---------------------------------------------------------------------*
*& Report zscr_exception
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_exception.

*CLASS bad_execution DEFINITION INHERITING FROM cx_static_check.
*ENDCLASS.
*
*CLASS bad_calculation DEFINITION INHERITING FROM cx_static_check.
*ENDCLASS.


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

CLASS calculator DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING expression TYPE string.
    METHODS calculate RETURNING VALUE(result) TYPE zcalc_result
                      RAISING   zcx_bad_calculation.

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
      WRITE 'Invalid operation.'.
    ENDIF.
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
  ENDTRY.
