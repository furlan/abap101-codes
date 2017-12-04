*&---------------------------------------------------------------------*
*& Report zrefactor_v2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrefactor_v2 LINE-SIZE 90.

TYPES: BEGIN OF lty_product,
         id          TYPE c LENGTH 5,
         description TYPE c LENGTH 30,
         quantity    TYPE n LENGTH 3,
         unit_price  TYPE p LENGTH 5 DECIMALS 2,
       END OF lty_product.

INTERFACE output_generator DEFERRED.
CLASS product DEFINITION DEFERRED.

TYPES items_table TYPE TABLE OF REF TO product.

CLASS product DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING imc_product TYPE lty_product.

    METHODS set IMPORTING im_product TYPE lty_product.

    METHODS get RETURNING VALUE(re_product) TYPE lty_product.

    METHODS get_value RETURNING VALUE(re_value) TYPE zcalc_result.

  PRIVATE SECTION.
    DATA: product TYPE lty_product.

ENDCLASS.

CLASS product IMPLEMENTATION.

  METHOD constructor.
    me->set( imc_product ).
  ENDMETHOD.

  METHOD set.
    me->product = im_product.
  ENDMETHOD.

  METHOD get.
    re_product = me->product.
  ENDMETHOD.

  METHOD get_value.
    re_value = me->product-unit_price * me->product-quantity.
  ENDMETHOD.
ENDCLASS.

CLASS test_product DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA test_product TYPE REF TO product.
    METHODS product_total FOR TESTING.

ENDCLASS.

CLASS test_product IMPLEMENTATION.
  METHOD product_total.
    DATA product_data TYPE lty_product.
    product_data-id = '025'.
    product_data-description = 'Cellphone 3000'.
    product_data-quantity = 3.
    product_data-unit_price = 1400.

    CREATE OBJECT me->test_product EXPORTING imc_product = product_data.
    DATA(product_value) = me->test_product->get_value( ).
    cl_abap_unit_assert=>assert_equals( act = product_value exp = 4200 ).
  ENDMETHOD.
ENDCLASS.


CLASS purchase_order DEFINITION.

  PUBLIC SECTION.
    METHODS add_item IMPORTING im_item TYPE REF TO product.

    METHODS get_po_total RETURNING VALUE(re_total) TYPE lty_product-unit_price.

    METHODS get_items EXPORTING items_list TYPE items_table.

    METHODS link_display_generator IMPORTING generator_obj TYPE REF TO output_generator.

  PRIVATE SECTION.
    DATA items_list TYPE items_table.
    DATA display_generator TYPE REF TO output_generator.

ENDCLASS.

CLASS purchase_order IMPLEMENTATION.

  METHOD add_item.
    APPEND im_item TO items_list.
  ENDMETHOD.

  METHOD get_po_total.
    DATA: r_product  TYPE REF TO product,
          wa_product TYPE lty_product,
          vg_total   TYPE lty_product-unit_price.
    LOOP AT items_list INTO r_product.

      wa_product = r_product->get( ).
      vg_total = wa_product-unit_price * wa_product-quantity.
      ADD vg_total TO re_total.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_items.
    items_list = me->items_list.
  ENDMETHOD.

  METHOD link_display_generator.
    me->display_generator = generator_obj.
  ENDMETHOD.

ENDCLASS.

CLASS test_purchase_order DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA test_purchase_order TYPE REF TO purchase_order.
    METHODS setup.
    METHODS return_total_po FOR TESTING.
    METHODS should_not_have_price_zeroless FOR TESTING.
ENDCLASS.

CLASS test_purchase_order IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->test_purchase_order.
  ENDMETHOD.

  METHOD return_total_po.

    DATA product_data TYPE lty_product.
    DATA test_product TYPE REF TO product.

    product_data-id = '025'.
    product_data-description = 'Cellphone 3000'.
    product_data-quantity = 3.
    product_data-unit_price = 1400.

    CREATE OBJECT test_product
      EXPORTING
        imc_product = product_data.
    me->test_purchase_order->add_item( test_product ).

    product_data-id = '984'.
    product_data-description = 'TV 40pol'.
    product_data-quantity = 6.
    product_data-unit_price = 3400.

    CREATE OBJECT test_product
      EXPORTING
        imc_product = product_data.
    me->test_purchase_order->add_item( test_product ).

    product_data-id = '758'.
    product_data-description = 'Audio System 439'.
    product_data-quantity = 2.
    product_data-unit_price = 7800.

    CREATE OBJECT test_product
      EXPORTING
        imc_product = product_data.
    me->test_purchase_order->add_item( test_product ).

    DATA(po_total) = me->test_purchase_order->get_po_total( ).
    cl_abap_unit_assert=>assert_equals( act = po_total exp = 40200 ).

  ENDMETHOD.

  METHOD should_not_have_price_zeroless.
    DATA product_data TYPE lty_product.
    DATA test_product TYPE REF TO product.
    DATA test_exception TYPE REF TO cx_static_check.

    product_data-id = '025'.
    product_data-description = 'Cellphone 3000'.
    product_data-quantity = 3.
    product_data-unit_price = 0.

    CREATE OBJECT test_product
      EXPORTING
        imc_product = product_data.
    me->test_purchase_order->add_item( test_product ).

    DATA(po_total) = me->test_purchase_order->get_po_total( ).
    cl_abap_unit_assert=>assert_equals( act = po_total exp = 0 ).

  ENDMETHOD.
ENDCLASS.

INTERFACE output_generator.
  METHODS generate IMPORTING po_object TYPE REF TO purchase_order.
ENDINTERFACE.

CLASS report_list DEFINITION.

  PUBLIC SECTION.
    INTERFACES output_generator.

ENDCLASS.

CLASS report_list IMPLEMENTATION.
  METHOD output_generator~generate.
    DATA: r_product  TYPE REF TO product,
          wa_product TYPE lty_product,
          vg_total   TYPE lty_product-unit_price,
          vg_total_p TYPE lty_product-unit_price.

    DATA items_list TYPE items_table.

    po_object->get_items( IMPORTING items_list = items_list ).

    LOOP AT items_list INTO r_product.
      AT FIRST.
        FORMAT COLOR COL_HEADING.
        WRITE:  /1 'ID',
                5 'Description',
                30 'Quant.',
                60 'Unit Price',
                80 'Total'.
        FORMAT COLOR OFF.
        ULINE.
      ENDAT.

      wa_product = r_product->get( ).
      vg_total = wa_product-unit_price * wa_product-quantity.

      WRITE: /1  wa_product-id,
              5  wa_product-description,
              30 wa_product-quantity,
              60 wa_product-unit_price,
              80 vg_total.

      ADD vg_total TO vg_total_p.

      AT LAST.
        ULINE.
        FORMAT COLOR 7.
        WRITE: / 'Total of Purchase Order --> ', vg_total_p.
      ENDAT.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

DATA: r_product  TYPE REF TO product,
      r_pur_ord  TYPE REF TO purchase_order,
      wa_product TYPE lty_product.

START-OF-SELECTION.

  CREATE OBJECT r_pur_ord.

  wa_product-id = '025'.
  wa_product-description = 'Cellphone 3000'.
  wa_product-quantity = 3.
  wa_product-unit_price = 1400.

  CREATE OBJECT r_product
    EXPORTING
      imc_product = wa_product.

  r_pur_ord->add_item( r_product ).
  wa_product-id = '984'.
  wa_product-description = 'TV 40pol'.
  wa_product-quantity = 6.
  wa_product-unit_price = 3400.

  CREATE OBJECT r_product
    EXPORTING
      imc_product = wa_product.
  r_pur_ord->add_item( r_product ).

  wa_product-id = '758'.
  wa_product-description = 'Audio System 439'.
  wa_product-quantity = 2.
  wa_product-unit_price = 7800.

  CREATE OBJECT r_product
    EXPORTING
      imc_product = wa_product.
  r_pur_ord->add_item( r_product ).

  DATA out_display TYPE REF TO output_generator.
  out_display = NEW report_list( ) .
  out_display->generate( r_pur_ord ).
