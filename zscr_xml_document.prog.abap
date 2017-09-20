*&---------------------------------------------------------------------*
*& Report zscr_xml_document
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zscr_xml_document.

CLASS xml_document DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create_from_scratch RETURNING VALUE(xml_document) TYPE REF TO xml_document.

    CLASS-METHODS create_from_file IMPORTING filename            TYPE string
                                   RETURNING VALUE(xml_document) TYPE REF TO xml_document.

    CLASS-METHODS create_from_stream IMPORTING stream              TYPE xstring
                                     RETURNING VALUE(xml_document) TYPE REF TO xml_document.

  PRIVATE SECTION.

    METHODS constructor.

ENDCLASS.

CLASS xml_document IMPLEMENTATION.

  METHOD constructor.
    " default initialization code goes here...
  ENDMETHOD.

  METHOD create_from_scratch.
    CREATE OBJECT xml_document.
  ENDMETHOD.

  METHOD create_from_file.
    CREATE OBJECT xml_document.
  ENDMETHOD.

  METHOD create_from_stream.
    CREATE OBJECT xml_document.
  ENDMETHOD.

ENDCLASS.

DATA some_xml_document TYPE REF TO xml_document.

START-OF-SELECTION.

  some_xml_document = xml_document=>create_from_scratch( ).
