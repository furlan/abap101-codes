*&---------------------------------------------------------------------*
*& Report zpo_sample_data_loader
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpo_sample_data_loader.

PARAMETERS p_exec AS CHECKBOX.

DATA poh_data TYPE zpoheader.
DATA poh_list TYPE TABLE OF zpoheader.
DATA po_data TYPE zpoitems.
DATA po_list TYPE TABLE OF zpoitems.
DATA product_data TYPE zproducts.
DATA product_list TYPE TABLE OF zproducts.

START-OF-SELECTION.

  CHECK p_exec = 'X'.

  CLEAR product_data.
  product_data-id = '025'.
  product_data-description = 'Cellphone 3000'.
  product_data-stock_quant = 3.
  product_data-uom = 'UN'.
  product_data-std_unit_price = 1400.
  product_data-currency = 'USD'.
  APPEND product_data TO product_list.

  CLEAR product_data.
  product_data-id = '984'.
  product_data-description = 'TV 40pol'.
  product_data-stock_quant = 6.
  product_data-uom = 'UN'.
  product_data-std_unit_price = 3400.
  product_data-currency = 'USD'.
  APPEND product_data TO product_list.

  CLEAR product_data.
  product_data-id = '758'.
  product_data-description = 'Audio System 439'.
  product_data-stock_quant = 2.
  product_data-uom = 'UN'.
  product_data-std_unit_price = 7800.
  product_data-currency = 'USD'.
  APPEND product_data TO product_list.

  CLEAR product_data.
  product_data-id = '026'.
  product_data-description = 'Cellphone 4000'.
  product_data-stock_quant = 2.
  product_data-uom = 'UN'.
  product_data-std_unit_price = 1800.
  product_data-currency = 'USD'.
  APPEND product_data TO product_list.

  CLEAR product_data.
  product_data-id = '977'.
  product_data-description = 'TV 72pol'.
  product_data-stock_quant = 4.
  product_data-uom = 'UN'.
  product_data-std_unit_price = 4700.
  APPEND product_data TO product_list.

  CLEAR product_data.
  product_data-id = '558'.
  product_data-description = 'Audio System 5000'.
  product_data-stock_quant = 6.
  product_data-uom = 'UN'.
  product_data-std_unit_price = 5455.
  product_data-currency = 'USD'.
  APPEND product_data TO product_list.

  DELETE FROM zproducts.
  INSERT zproducts FROM TABLE product_list.

  CLEAR poh_data.
  poh_data-ponum = '00001'.
  poh_data-created_on = '20171119'.
  poh_data-created_by = sy-uname.
  poh_data-vendor = 'Good Buy'.
  poh_data-status = 2.
  APPEND poh_data TO poh_list.

  CLEAR po_data.
  po_data-ponum = poh_data-ponum.
  po_data-poline = 1.
  po_data-product_id = '758'.
  po_data-quantity = 1.
  po_data-uom = 'UN'.
  po_data-unit_price = 7800.
  po_data-currency = 'USD'.
  APPEND po_data TO po_list.

  CLEAR po_data.
  po_data-ponum = poh_data-ponum.
  po_data-poline = 2.
  po_data-product_id = '977'.
  po_data-quantity = 1.
  po_data-uom = 'UN'.
  po_data-unit_price = 4600.
  po_data-currency = 'USD'.
  APPEND po_data TO po_list.

  CLEAR po_data.
  po_data-ponum = poh_data-ponum.
  po_data-poline = 3.
  po_data-product_id = '758'.
  po_data-quantity = 2.
  po_data-uom = 'UN'.
  po_data-unit_price = 5355.
  po_data-currency = 'USD'.
  APPEND po_data TO po_list.

  CLEAR poh_data.
  poh_data-ponum = '00002'.
  poh_data-created_on = '20171121'.
  poh_data-created_by = sy-uname.
  poh_data-vendor = 'Macro Eletro'.
  poh_data-status = 8.
  APPEND poh_data TO poh_list.

  CLEAR po_data.
  po_data-ponum = poh_data-ponum.
  po_data-poline = 1.
  po_data-product_id = '025'.
  po_data-quantity = 1.
  po_data-uom = 'UN'.
  po_data-unit_price = 1350.
  po_data-currency = 'USD'.
  APPEND po_data TO po_list.

  CLEAR po_data.
  po_data-ponum = poh_data-ponum.
  po_data-poline = 2.
  po_data-product_id = '026'.
  po_data-quantity = 1.
  po_data-uom = 'UN'.
  po_data-unit_price = 1800.
  po_data-currency = 'USD'.
  APPEND po_data TO po_list.

  DELETE FROM zpoheader.
  INSERT zpoheader FROM TABLE poh_list.

  DELETE FROM zpoitems.
  INSERT zpoitems FROM TABLE po_list.

  COMMIT WORK.

  WRITE 'Data load completed!'.
