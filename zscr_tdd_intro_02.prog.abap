*&---------------------------------------------------------------------*
*& Report ZSCR_TDD_INTRO_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_tdd_intro_02.

CLASS product_list DEFINITION.

  PUBLIC SECTION.

    METHODS constructor IMPORTING name TYPE string.

    METHODS add_product IMPORTING product_name  TYPE string
                                  product_price TYPE i.

    METHODS get_max RETURNING VALUE(product_name) TYPE string.

  PRIVATE SECTION.

    DATA name TYPE string.

    DATA: BEGIN OF product,
            name  TYPE string,
            price TYPE i,
          END OF product.

    DATA product_list LIKE TABLE OF product.
ENDCLASS.

CLASS product_list IMPLEMENTATION.

  METHOD constructor. me->name = name. ENDMETHOD.

  METHOD add_product.
    me->product-name = product_name. me->product-price = product_price.

    APPEND me->product TO me->product_list.
  ENDMETHOD.

  METHOD get_max.

    DATA product_price TYPE i.

    LOOP AT me->product_list INTO me->product.

      IF  product_price < me->product-price.
        product_price = me->product-price.
        product_name = me->product-name.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS test_max_value DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA list_test TYPE REF TO product_list.
    DATA error_message TYPE string.
    DATA product_max_value TYPE string.
    METHODS setup.
    METHODS test_mixed_order FOR TESTING.
    METHODS test_ascending_order FOR TESTING.

ENDCLASS.

CLASS test_max_value IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT list_test
      EXPORTING
        name = 'Test list'.
  ENDMETHOD.

  METHOD test_mixed_order.
    me->list_test->add_product( product_name = 'Liquidificador' product_price = 250 ).
    me->list_test->add_product( product_name = 'Geladeira' product_price = 450 ).
    me->list_test->add_product( product_name = 'Jogo de pratos' product_price = 70 ).

    me->product_max_value = me->list_test->get_max( ).

    CONCATENATE 'Error getting max value: '
                'expected: '
                'Geladeira'
                ', actual: '
                me->product_max_value
        INTO me->error_message.

    cl_aunit_assert=>assert_equals(
        act = me->product_max_value
        exp = 'Geladeira'
        msg = me->error_message
    ).
  ENDMETHOD.

  METHOD test_ascending_order.
    me->list_test->add_product( product_name = 'Jogo de pratos' product_price = 70 ).
    me->list_test->add_product( product_name = 'Liquidificador' product_price = 250 ).
    me->list_test->add_product( product_name = 'Geladeira' product_price = 450 ).

    me->product_max_value = me->list_test->get_max( ).

    CONCATENATE 'Error getting max value: '
                'expected: '
                'Geladeira'
                ', actual: '
                me->product_max_value
        INTO me->error_message.

    cl_aunit_assert=>assert_equals(
        act = me->product_max_value
        exp = 'Geladeira'
        msg = me->error_message
    ).
  ENDMETHOD.

ENDCLASS.
