*&---------------------------------------------------------------------*
*& Report zscr_interface
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_interface.

INTERFACE comparable.
  METHODS comparable_to IMPORTING object_compare TYPE REF TO object
                        RETURNING VALUE(result)  TYPE i.
ENDINTERFACE.

CLASS customer DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING customer_name TYPE string.
    METHODS get_name RETURNING VALUE(name) TYPE string.
    INTERFACES comparable.

  PRIVATE SECTION.
    CLASS-DATA next_id TYPE i.
    DATA id TYPE i.
    DATA name TYPE string.

ENDCLASS.

CLASS customer IMPLEMENTATION.

  METHOD constructor.
    ADD 1 TO next_id.
    me->id = next_id.
    me->name = customer_name.
  ENDMETHOD.

  METHOD get_name.
    name = me->name.
  ENDMETHOD.

  METHOD comparable~comparable_to.
    DATA input_customer TYPE REF TO customer.

    input_customer ?= object_compare.

    IF me->id > input_customer->id.
      result = 1.
    ELSEIF me->id < input_customer->id.
      result = -1.
    ELSE.
      result = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS vector DEFINITION.

  PUBLIC SECTION.

    METHODS add IMPORTING add_object TYPE REF TO object.
    METHODS remove IMPORTING remove_index TYPE i.
    METHODS sort.
    METHODS get_iterator RETURNING VALUE(iterator) TYPE REF TO cl_swf_utl_iterator.

  PRIVATE SECTION.
    DATA elements TYPE TABLE OF REF TO object.
ENDCLASS.

CLASS vector IMPLEMENTATION.
  METHOD add.
    APPEND add_object TO me->elements.
  ENDMETHOD.

  METHOD remove.
    DELETE me->elements INDEX remove_index.
  ENDMETHOD.

  METHOD get_iterator.
    CREATE OBJECT iterator EXPORTING im_object_list = me->elements.
  ENDMETHOD.

  METHOD sort.
    " Insertion Sort
    DATA element_move TYPE REF TO comparable.

    LOOP AT me->elements INTO DATA(element) FROM 2.
      element_move ?= element.
      DATA(j) = sy-tabix - 1.

      WHILE j > 0 AND element_move->comparable_to( me->elements[ j ] ) = -1.
        DATA(element_swap) = me->elements[ j ].
        me->elements[ j ] = element_move.
        me->elements[ j + 1 ] = element_swap.
        j = j - 1.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

DATA my_customer1 TYPE REF TO customer.
DATA my_customer2 TYPE REF TO customer.
DATA my_customer3 TYPE REF TO customer.
DATA my_customer4 TYPE REF TO customer.
DATA my_customer5 TYPE REF TO customer.
DATA my_customer6 TYPE REF TO customer.
DATA my_vector TYPE REF TO vector.

START-OF-SELECTION.

  CREATE OBJECT my_vector.

  CREATE OBJECT my_customer1 EXPORTING customer_name = 'Customer 1'.
  CREATE OBJECT my_customer2 EXPORTING customer_name = 'Customer 2'.
  CREATE OBJECT my_customer3 EXPORTING customer_name = 'Customer 3'.
  CREATE OBJECT my_customer4 EXPORTING customer_name = 'Customer 4'.
  CREATE OBJECT my_customer5 EXPORTING customer_name = 'Customer 5'.
  CREATE OBJECT my_customer6 EXPORTING customer_name = 'Customer 6'.

  my_vector->add( my_customer6 ).
  my_vector->add( my_customer3 ).
  my_vector->add( my_customer1 ).
  my_vector->add( my_customer2 ).
  my_vector->add( my_customer5 ).
  my_vector->add( my_customer4 ).

  WRITE / 'Original order:'.

  DATA(iterator) = my_vector->get_iterator( ).
  DATA current_customer TYPE REF TO customer.

  DO iterator->get_count( ) TIMES.
    DATA(current_object) = iterator->get_current( ).
    current_customer ?= current_object.
    WRITE: / current_customer->get_name( ).
    iterator->get_next( ).
  ENDDO.

  WRITE / 'Sorted order'.

  my_vector->sort( ).

  iterator = my_vector->get_iterator( ).
  DO iterator->get_count( ) TIMES.
    current_object = iterator->get_current( ).
    current_customer ?= current_object.
    WRITE: / current_customer->get_name( ).
    iterator->get_next( ).
  ENDDO.
