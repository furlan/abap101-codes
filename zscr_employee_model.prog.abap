*&---------------------------------------------------------------------*
*& Report zscr_employee_model
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_employee_model.

CLASS employee DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS constructor IMPORTING employee_id        TYPE i
                                  employee_name      TYPE string
                                  employee_hire_date TYPE d.

    METHODS calculate_wage ABSTRACT RETURNING VALUE(wage_amount) TYPE i.

  PROTECTED SECTION.
    DATA last_payment TYPE d.

  PRIVATE SECTION.
    DATA id TYPE i.
    DATA name TYPE string.
    DATA hire_date TYPE d.


ENDCLASS.

CLASS employee IMPLEMENTATION.

  METHOD constructor.
    me->id = employee_id.
    me->name = employee_name.
    me->hire_date = employee_hire_date.
  ENDMETHOD.

ENDCLASS.

CLASS hourly_employee DEFINITION INHERITING FROM employee.
  PUBLIC SECTION.
    METHODS constructor IMPORTING hourly_employee_id        TYPE i
                                  hourly_employee_name      TYPE string
                                  hourly_employee_hire_date TYPE d
                                  hourly_employee_wage_rate TYPE i.

    METHODS calculate_wage REDEFINITION.

  PRIVATE SECTION.
    DATA wage_rate TYPE i.

ENDCLASS.

CLASS hourly_employee IMPLEMENTATION.
  METHOD constructor.
    super->constructor( employee_id = hourly_employee_id
                        employee_name = hourly_employee_name
                        employee_hire_date = hourly_employee_hire_date ).
    me->wage_rate = hourly_employee_wage_rate.
  ENDMETHOD.

  METHOD calculate_wage.
    IF me->wage_rate = 0.
      wage_amount = 0.
    ELSE.
      wage_amount = ( sy-datum - me->last_payment ) * me->wage_rate.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
