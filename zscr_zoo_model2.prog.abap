*&---------------------------------------------------------------------*
*& Report zscr_zoo_model2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_zoo_model2.

INTERFACE speaker.
  METHODS speak RETURNING VALUE(animal_noise) TYPE string.
ENDINTERFACE.

CLASS animal DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS constructor IMPORTING animal_name TYPE string.
    METHODS get_animal_name RETURNING VALUE(animal_name) TYPE string.
    INTERFACES speaker.

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

  METHOD speaker~speak.
    animal_noise = me->name && ' but, does animal talk!?'.
  ENDMETHOD.
ENDCLASS.

CLASS duck DEFINITION INHERITING FROM animal.

  PUBLIC SECTION.
    METHODS speaker~speak REDEFINITION.
    METHODS fly.

ENDCLASS.

CLASS duck IMPLEMENTATION.
  METHOD speaker~speak.
    animal_noise = 'Quack!'.
  ENDMETHOD.

  METHOD fly.
    WRITE / 'Ducks can fly!'.
  ENDMETHOD.
ENDCLASS.

CLASS bunny DEFINITION INHERITING FROM animal.

  PUBLIC SECTION.
    METHODS speaker~speak REDEFINITION.
    METHODS jump.

ENDCLASS.

CLASS bunny IMPLEMENTATION.
  METHOD speaker~speak.
    animal_noise = 'Whats up, doc?'.
  ENDMETHOD.

  METHOD jump.
    WRITE / 'Bunnies can jump!'.
  ENDMETHOD.
ENDCLASS.

CLASS stone DEFINITION.

  PUBLIC SECTION.
    INTERFACES speaker.
    METHODS constructor IMPORTING stone_weight TYPE i.
    METHODS roll.

  PRIVATE SECTION.
    DATA weight TYPE i.

ENDCLASS.

CLASS stone IMPLEMENTATION.
  METHOD constructor.
    me->weight = stone_weight.
  ENDMETHOD.

  METHOD speaker~speak.
    animal_noise = 'Stones does not speak, they roll!'.
  ENDMETHOD.

  METHOD roll.
    WRITE / 'I''am a rolling stone!'.
  ENDMETHOD.

ENDCLASS.


CLASS zoo DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING zoo_name TYPE string.
    METHODS add_speaker IMPORTING speaker_added TYPE REF TO speaker.
    METHODS make_noise.

  PRIVATE SECTION.
    DATA name TYPE string.
    DATA list_speaker TYPE TABLE OF REF TO speaker.

ENDCLASS.

CLASS zoo IMPLEMENTATION.
  METHOD constructor.
    me->name = zoo_name.
  ENDMETHOD.

  METHOD add_speaker.
    APPEND speaker_added TO me->list_speaker.
  ENDMETHOD.

  METHOD make_noise.
    DATA speaker_from_list TYPE REF TO speaker.
    LOOP AT me->list_speaker INTO speaker_from_list.
      WRITE / speaker_from_list->speak( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

DATA my_zoo TYPE REF TO zoo.
DATA my_duck TYPE REF TO duck.
DATA my_bunny TYPE REF TO bunny.
DATA my_stone TYPE REF TO stone.

START-OF-SELECTION.

  CREATE OBJECT my_zoo EXPORTING zoo_name = 'SAP Project'.
  CREATE OBJECT my_duck EXPORTING animal_name = 'Patolino'.
  my_zoo->add_speaker( my_duck ).

  CREATE OBJECT my_bunny EXPORTING animal_name = 'Pernalonga'.
  my_zoo->add_speaker( my_bunny ).

  CREATE OBJECT my_stone EXPORTING stone_weight = 10.
  my_zoo->add_speaker( my_stone ).

  my_zoo->make_noise( ).

  DATA my_speaker TYPE REF TO speaker.
  CREATE OBJECT my_speaker TYPE stone EXPORTING stone_weight = 10.
  WRITE / my_speaker->speak( ).

  DATA my_stone2 TYPE REF TO stone.
  my_stone2 ?= my_speaker.

  my_stone2->roll( ).
