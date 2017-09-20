*&---------------------------------------------------------------------*
*& Report zscr_romans
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_romans.

CLASS romans_conversor DEFINITION.
  PUBLIC SECTION.
    METHODS convert IMPORTING roman_number TYPE string
                    RETURNING VALUE(value) TYPE i.

  PRIVATE SECTION.
    METHODS get_number IMPORTING roman_algarism TYPE string
                       RETURNING VALUE(value)   TYPE i.
ENDCLASS.

CLASS romans_conversor IMPLEMENTATION.
  METHOD convert.
    DATA pos TYPE i.
    DATA next_number TYPE i.

    WHILE pos < strlen( roman_number ).
      DATA(roman_algarism) = roman_number+pos(1).
      DATA(next_pos) = pos + 1.
      TRY.
          DATA(next_algarism) = roman_number+next_pos(1).
        CATCH cx_sy_range_out_of_bounds.
          next_algarism = roman_algarism.
      ENDTRY.

      IF me->get_number( roman_algarism ) >= me->get_number( next_algarism ).
        value = value + me->get_number( roman_algarism ).
      ELSE.
        value = value - me->get_number( roman_algarism ).
      ENDIF.
      ADD 1 TO pos.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_number.
    CASE roman_algarism.
      WHEN 'I'.
        value = 1.
      WHEN 'V'.
        value = 5.
      WHEN 'X'.
        value = 10.
      WHEN 'L'.
        value = 50.
      WHEN 'C'.
        value = 100.
      WHEN 'D'.
        value = 500.
      WHEN 'M'.
        value = 1000.
      WHEN OTHERS.
        value = 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS test_romans_conversor DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA conversor TYPE REF TO romans_conversor.
    METHODS setup.
    METHODS must_understand_symbol_i FOR TESTING.
    METHODS must_understand_symbol_v FOR TESTING.
    METHODS must_und_repeated_symbols_ii FOR TESTING.
    METHODS must_und_repeated_symbols_xxii FOR TESTING.
    METHODS must_understand_ix FOR TESTING.
    METHODS must_understand_xxiv FOR TESTING.
ENDCLASS.

CLASS test_romans_conversor IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->conversor.
  ENDMETHOD.

  METHOD must_understand_symbol_i.
    DATA(value) = me->conversor->convert( 'I' ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = value ).
  ENDMETHOD.

  METHOD must_understand_symbol_v.
    DATA(value) = me->conversor->convert( 'V' ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = value ).
  ENDMETHOD.

  METHOD must_und_repeated_symbols_ii.
    DATA(value) = me->conversor->convert( 'II' ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = value ).
  ENDMETHOD.

  METHOD must_und_repeated_symbols_xxii.
    DATA(value) = me->conversor->convert( 'XXII' ).
    cl_abap_unit_assert=>assert_equals( exp = 22 act = value ).
  ENDMETHOD.

  METHOD must_understand_ix.
    DATA(value) = me->conversor->convert( 'IX' ).
    cl_abap_unit_assert=>assert_equals( exp = 9 act = value ).
  ENDMETHOD.

  METHOD must_understand_xxiv.
    DATA(value) = me->conversor->convert( 'XXIV' ).
    cl_abap_unit_assert=>assert_equals( exp = 24 act = value ).
  ENDMETHOD.

ENDCLASS.

DATA conversor TYPE REF TO romans_conversor.

PARAMETERS roman TYPE string.

START-OF-SELECTION.

  CREATE OBJECT conversor.
  WRITE: / roman, '=', conversor->convert( roman ).
