*&---------------------------------------------------------------------*
*& Report zscr_tdd_class_design
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_tdd_class_design.

CLASS item DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING item_description TYPE string
                                  item_quantity    TYPE i
                                  item_unit_price  TYPE zcalc_result.

    METHODS get_price RETURNING VALUE(value) TYPE zcalc_result.

  PRIVATE SECTION.
    DATA description TYPE string.
    DATA quantity TYPE i.
    DATA unit_price TYPE zcalc_result.

ENDCLASS.

CLASS item IMPLEMENTATION.
  METHOD constructor.
    me->description = item_description.
    me->quantity = item_quantity.
    me->unit_price = item_unit_price.
  ENDMETHOD.

  METHOD get_price.
    value = me->unit_price * me->quantity.
  ENDMETHOD.
ENDCLASS.

CLASS shopping_cart DEFINITION.
  PUBLIC SECTION.
    TYPES item_tab TYPE TABLE OF REF TO item WITH NON-UNIQUE DEFAULT KEY.
    METHODS add IMPORTING add_item TYPE REF TO item .
    METHODS get_items RETURNING VALUE(items) TYPE item_tab.
    METHODS get_biggest_price RETURNING VALUE(value) TYPE zcalc_result.

  PRIVATE SECTION.
    DATA items TYPE item_tab.
ENDCLASS.

CLASS shopping_cart IMPLEMENTATION.
  METHOD add.
    APPEND add_item TO me->items.
  ENDMETHOD.

  METHOD get_items.
    items = me->items.
  ENDMETHOD.

  METHOD get_biggest_price.
    IF lines( me->items ) EQ 0.
      value = 0.
    ELSE.
      DATA(biggest) = me->items[ 1 ]->get_price( ).
      LOOP AT me->items INTO DATA(item).
        IF biggest < item->get_price( ).
          biggest = item->get_price( ).
        ENDIF.
      ENDLOOP.
      value = biggest.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*CLASS biggest_price DEFINITION.
*  PUBLIC SECTION.
*    METHODS get IMPORTING cart         TYPE REF TO shopping_cart
*                RETURNING VALUE(value) TYPE zcalc_result.
*ENDCLASS.
*
*CLASS biggest_price IMPLEMENTATION.
*  METHOD get.
*    DATA(items) = cart->get_items( ).
*    IF lines( items ) EQ 0.
*      value = 0.
*    ELSE.
*      DATA(biggest) = items[ 1 ]->get_price( ).
*      LOOP AT items INTO DATA(item).
*        IF biggest < item->get_price( ).
*          biggest = item->get_price( ).
*        ENDIF.
*      ENDLOOP.
*      value = biggest.
*    ENDIF.
*  ENDMETHOD.
*ENDCLASS.

CLASS test_biggest_price DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA test_cart TYPE REF TO shopping_cart.
    METHODS setup.
    METHODS should_return_zero_empty_cart FOR TESTING.
    METHODS should_return_value_1_item FOR TESTING.
    METHODS should_ret_biggest_n_items FOR TESTING.
ENDCLASS.

CLASS test_biggest_price IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->test_cart.
  ENDMETHOD.

  METHOD should_return_zero_empty_cart.
    DATA(actual) = me->test_cart->get_biggest_price( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = actual ).
  ENDMETHOD.

  METHOD should_return_value_1_item.
    DATA(item) = NEW item( item_description = 'Geladeira'
                           item_quantity = 1
                           item_unit_price = 900 ).
    me->test_cart->add( item ).
    DATA(actual) = me->test_cart->get_biggest_price( ).
    cl_abap_unit_assert=>assert_equals( exp = 900 act = actual ).
  ENDMETHOD.

  METHOD should_ret_biggest_n_items.
    DATA(item) = NEW item( item_description = 'Geladeira'
                           item_quantity = 1
                           item_unit_price = 900 ).
    me->test_cart->add( item ).

    item = NEW item( item_description = 'Fogão'
                       item_quantity = 1
                       item_unit_price = 1500 ).
    me->test_cart->add( item ).

    item = NEW item( item_description = 'Máquina de Lavar'
                       item_quantity = 1
                       item_unit_price = 750 ).
    me->test_cart->add( item ).

    DATA(actual) = me->test_cart->get_biggest_price( ).
    cl_abap_unit_assert=>assert_equals( exp = 1500 act = actual ).
  ENDMETHOD.

ENDCLASS.


















* eof
