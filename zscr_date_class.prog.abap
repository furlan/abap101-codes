*&---------------------------------------------------------------------*
*& Report zscr_date_class
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_date_class.

CLASS time DEFINITION DEFERRED.

CLASS date DEFINITION FRIENDS time.

  PUBLIC SECTION.
    METHODS set_month IMPORTING  month TYPE numc2
                      EXCEPTIONS invalid_date.

    METHODS set_day IMPORTING  day TYPE numc2
                    EXCEPTIONS invalid_date.

  PRIVATE SECTION.
    DATA day TYPE numc2.
    DATA month TYPE numc2.
    DATA year TYPE numc4.

ENDCLASS.

CLASS date IMPLEMENTATION.
  METHOD set_month.
    IF month LT 1 OR month GT 12.
      RAISE invalid_date.
    ENDIF.
    me->month = month.
  ENDMETHOD.

  METHOD set_day.
    DATA month_end TYPE numc2.

    CASE me->month.

      WHEN 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12.
        month_end = 31.
      WHEN 4 OR 6 OR 9 OR 11.
        month_end = 30.
      WHEN 2.
        month_end = 28.
      WHEN OTHERS.
        RAISE invalid_date.
    ENDCASE.

    IF day LT 1 OR day GT month_end.
      RAISE invalid_date.
    ELSE.
      me->day = day.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS time DEFINITION.

  PUBLIC SECTION.
    METHODS get_full_timestamp RETURNING VALUE(full_timestamp) TYPE tstamp.

ENDCLASS.

CLASS time IMPLEMENTATION.
  METHOD get_full_timestamp.
    DATA today TYPE REF TO date.

    CREATE OBJECT today.
    today->set_month( 11 ).
    today->set_day( 28 ).

    CONCATENATE today->day
                today->month
                today->year
                sy-uzeit
           INTO full_timestamp.

  ENDMETHOD.
ENDCLASS.

DATA initial_date TYPE REF TO date.
DATA final_date TYPE REF TO date.

DATA now TYPE REF TO time.

START-OF-SELECTION.

  CREATE OBJECT initial_date.
  initial_date->set_month( 11 ).
  initial_date->set_day( 28 ).

  CREATE OBJECT final_date.
  initial_date->set_month( 12 ).
  initial_date->set_day( 25 ).

  CREATE OBJECT now.
  WRITE: 'Timestamp Now!: ', now->get_full_timestamp( ).















  WRITE: 'EOF'.
