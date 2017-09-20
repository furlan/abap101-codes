*&---------------------------------------------------------------------*
*& Report zscr_zoo_model
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_zoo_model.

CLASS animal DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING animal_name TYPE string.
    METHODS get_animal_name RETURNING VALUE(animal_name) TYPE string.
    METHODS speak RETURNING VALUE(animal_noise) TYPE string.

  PRIVATE SECTION.
    DATA name TYPE string.

ENDCLASS.

CLASS animal IMPLEMENTATION.
  METHOD constructor.
    me->name = animal_name.
  ENDMETHOD.

  METHOD get_animal_name.
    animal_name = me->name.
  ENDMETHOD.

  METHOD speak.
    animal_noise = me->name && ' but, does animal talk?!'.
  ENDMETHOD.
ENDCLASS.

CLASS duck DEFINITION INHERITING FROM animal.

  PUBLIC SECTION.
    METHODS speak REDEFINITION.
    METHODS fly.

ENDCLASS.

CLASS duck IMPLEMENTATION.
  METHOD speak.
    animal_noise = 'Quack!'.
  ENDMETHOD.

  METHOD fly.
    WRITE / 'Ducks can fly!'.
  ENDMETHOD.
ENDCLASS.

CLASS bunny DEFINITION INHERITING FROM animal.

  PUBLIC SECTION.
    METHODS speak REDEFINITION.
    METHODS hop.

ENDCLASS.

CLASS bunny IMPLEMENTATION.
  METHOD speak.
    animal_noise = 'Whats up, Doc?'.
  ENDMETHOD.

  METHOD hop.
    WRITE / 'Bunnies can hop!'.
  ENDMETHOD.
ENDCLASS.

CLASS zoo DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING zoo_name TYPE string.
    METHODS add_animal IMPORTING animal_added TYPE REF TO animal.
    METHODS make_noise.

  PRIVATE SECTION.
    DATA name TYPE string.
    DATA list_animal TYPE TABLE OF REF TO animal.

ENDCLASS.

CLASS zoo IMPLEMENTATION.
  METHOD constructor.
    me->name = zoo_name.
  ENDMETHOD.

  METHOD add_animal.
    APPEND animal_added TO me->list_animal.
  ENDMETHOD.

  METHOD make_noise.
    DATA animal_from_list TYPE REF TO animal.
    LOOP AT me->list_animal INTO animal_from_list.
      WRITE / animal_from_list->speak( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

DATA my_duck TYPE REF TO duck.
DATA my_bunny TYPE REF TO bunny.
DATA my_zoo TYPE REF TO zoo.

START-OF-SELECTION.

  CREATE OBJECT my_zoo EXPORTING zoo_name = 'SAP Project'.
  CREATE OBJECT my_duck EXPORTING animal_name = 'Patolino'.
  my_zoo->add_animal( my_duck ).

  CREATE OBJECT my_bunny EXPORTING animal_name = 'Pernalonga'.
  my_zoo->add_animal( my_bunny ).

  my_zoo->make_noise( ).
