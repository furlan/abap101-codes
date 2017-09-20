*&---------------------------------------------------------------------*
*& Report zpoc_counter
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpoc_counter.

CLASS counter DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS increment.

    METHODS get_count RETURNING VALUE(actual_count) TYPE i.

  PRIVATE SECTION.

    CLASS-DATA count TYPE i.

ENDCLASS.

CLASS counter IMPLEMENTATION.

  METHOD class_constructor.
    count = 10.
  ENDMETHOD.

  METHOD increment.
    ADD 5 TO count.
  ENDMETHOD.

  METHOD get_count.
    actual_count = count.
  ENDMETHOD.

ENDCLASS.

DATA counter1 TYPE REF TO counter.
DATA counter2 TYPE REF TO counter.

START-OF-SELECTION.

  CREATE OBJECT counter1.
  WRITE: / counter1->get_count( ).

  DO 10 TIMES.
    counter1->increment( ).
  ENDDO.

  WRITE: / counter1->get_count( ).

  CREATE OBJECT counter2.
  counter2->increment( ).
  WRITE: / counter2->get_count( ).
