*&---------------------------------------------------------------------*
*& Report zrelped_v1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelped_v1 LINE-SIZE 90..

TYPES: BEGIN OF lty_product,
         product_id  TYPE c LENGTH 5,
         description TYPE c LENGTH 30,
         quantity    TYPE n LENGTH 3,
         unit_price  TYPE p LENGTH 5 DECIMALS 2,
       END OF lty_product.

INTERFACE output_generator DEFERRED.
CLASS product DEFINITION DEFERRED.

TYPES items_table TYPE TABLE OF REF TO product.

*----------------------------------------------------------------------*
*       CLASS lcl_product DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS product DEFINITION.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING imc_product TYPE lty_product.

    METHODS: set
      IMPORTING im_product TYPE lty_product,

      get
        RETURNING VALUE(re_product) TYPE lty_product.

  PRIVATE SECTION.
    DATA: product TYPE lty_product.

ENDCLASS.                    "lcl_product DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_product IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS product IMPLEMENTATION.

  METHOD constructor.
    me->set( imc_product ).
  ENDMETHOD.                    "constructor

  METHOD set.
    me->product = im_product.
  ENDMETHOD.                    "set

  METHOD get.
    re_product = me->product.
  ENDMETHOD.                    "get

ENDCLASS.                    "lcl_product IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_purchase_order DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS purchase_order DEFINITION.

  PUBLIC SECTION.
    METHODS: add_item IMPORTING im_item TYPE REF TO product,

      get_po_total RETURNING
                     VALUE(re_total) TYPE lty_product-unit_price,

      get_items EXPORTING items_list TYPE items_table.

  PRIVATE SECTION.
    DATA: items_list TYPE TABLE OF REF TO product.

ENDCLASS.                    "lcl_purchase_order DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_purchase_order IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS purchase_order IMPLEMENTATION.

  METHOD add_item.
    APPEND im_item TO items_list.
  ENDMETHOD.                    "add_item

  METHOD get_po_total.
    DATA: r_product  TYPE REF TO product,
          wa_product TYPE lty_product,
          vg_total   TYPE lty_product-unit_price.

    LOOP AT items_list INTO r_product.
      wa_product = r_product->get( ).
      vg_total = wa_product-unit_price * wa_product-quantity.
      ADD vg_total TO re_total.
    ENDLOOP.
  ENDMETHOD.                    "get_po_total

  METHOD get_items.
    items_list = me->items_list.
  ENDMETHOD.

ENDCLASS.                    "lcl_purchase_order IMPLEMENTATION

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

      WRITE: /1  wa_product-product_id,
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

  wa_product-product_id = '025'.
  wa_product-description = 'Cellphone 3000'.
  wa_product-quantity = 3.
  wa_product-unit_price = 1400.

  CREATE OBJECT r_product
    EXPORTING
      imc_product = wa_product.
  r_pur_ord->add_item( r_product ).

  wa_product-product_id = '984'.
  wa_product-description = 'TV 40pol'.
  wa_product-quantity = 6.
  wa_product-unit_price = 3400.

  CREATE OBJECT r_product
    EXPORTING
      imc_product = wa_product.
  r_pur_ord->add_item( r_product ).

  wa_product-product_id = '758'.
  wa_product-description = 'Audio System 439'.
  wa_product-quantity = 2.
  wa_product-unit_price = 7800.

  CREATE OBJECT r_product
    EXPORTING
      imc_product = wa_product.
  r_pur_ord->add_item( r_product ).

  DATA out_display TYPE REF TO output_generator.
  out_display = NEW report_list( ).
  out_display->generate( r_pur_ord ).
