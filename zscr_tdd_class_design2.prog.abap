*&---------------------------------------------------------------------*
*& Report zscr_tdd_class_design2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_tdd_class_design2.

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

CLASS shopping_cart_builder DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_empty_cart  RETURNING VALUE(empty_cart) TYPE REF TO shopping_cart.
    METHODS get_cart_1_item RETURNING VALUE(loaded_cart) TYPE REF TO shopping_cart.
    METHODS get_cart_3_items RETURNING VALUE(loaded_cart) TYPE REF TO shopping_cart.
  PRIVATE SECTION.
    DATA shopping_cart TYPE REF TO shopping_cart.
ENDCLASS.

CLASS shopping_cart_builder IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT me->shopping_cart.
  ENDMETHOD.

  METHOD get_empty_cart.
    empty_cart = me->shopping_cart.
  ENDMETHOD.

  METHOD get_cart_1_item.
    DATA(item) = NEW item( item_description = 'Geladeira'
                            item_quantity = 1
                            item_unit_price = 900 ).
    me->shopping_cart->add( item ).
    loaded_cart = me->shopping_cart.
  ENDMETHOD.

  METHOD get_cart_3_items.
    DATA(item) = NEW item( item_description = 'Geladeira'
                              item_quantity = 1
                              item_unit_price = 900 ).
    me->shopping_cart->add( item ).
    item = NEW item( item_description = 'Fogão'
                     item_quantity = 1
                     item_unit_price = 1500 ).
    me->shopping_cart->add( item ).
    item = NEW item( item_description = 'Máquina de Lavar'
                     item_quantity = 1
                     item_unit_price = 750 ).
    me->shopping_cart->add( item ).
    loaded_cart = me->shopping_cart.
  ENDMETHOD.
ENDCLASS.

CLASS test_biggest_price DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA test_cart TYPE REF TO shopping_cart.
    DATA cart_builder TYPE REF TO shopping_cart_builder.
    METHODS setup.
    METHODS should_return_zero_empty_cart FOR TESTING.
    METHODS should_return_value_1_item FOR TESTING.
    METHODS should_ret_biggest_n_items FOR TESTING.
ENDCLASS.

CLASS test_biggest_price IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->cart_builder.
    me->test_cart = me->cart_builder->get_empty_cart( ).
  ENDMETHOD.

  METHOD should_return_zero_empty_cart.
    DATA(actual) = me->test_cart->get_biggest_price( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = actual ).
  ENDMETHOD.

  METHOD should_return_value_1_item.
    me->test_cart = me->cart_builder->get_cart_1_item( ).
    DATA(actual) = me->test_cart->get_biggest_price( ).
    cl_abap_unit_assert=>assert_equals( exp = 900 act = actual ).
  ENDMETHOD.

  METHOD should_ret_biggest_n_items.
    me->test_cart = me->cart_builder->get_cart_3_items( ).
    DATA(actual) = me->test_cart->get_biggest_price( ).
    cl_abap_unit_assert=>assert_equals( exp = 1500 act = actual ).
  ENDMETHOD.

ENDCLASS.


















* eof
