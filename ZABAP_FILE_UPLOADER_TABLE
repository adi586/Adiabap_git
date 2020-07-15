class ZCL_ABAP_FILE_UPLOADER_TABLE definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
   DATA: tablename TYPE string.
     DATA: filename TYPE string.
     DATA: fileext TYPE string.
     DATA: dataoption TYPE string.
    DATA: filedata TYPE string.
 TYPES: BEGIN OF ty_content,
       file_content TYPE string,
     END OF ty_content.
  DATA: lt_file_cont TYPE STANDARD TABLE OF ty_content.
DATA:ls_file_cont TYPE ty_content.
     METHODS: get_input_field_value IMPORTING name         TYPE string
                                             dataref      TYPE data
                                    RETURNING VALUE(value) TYPE string.
     METHODS: get_html RETURNING VALUE(ui_html) TYPE string.

       METHODS: get_html_csv RETURNING VALUE(ui_html) TYPE string.

protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAP_FILE_UPLOADER_TABLE IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.

    CASE request->get_method(  ).

      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_table_request) = request->get_header_field( 'sap-table-request' ).
        IF sap_table_request IS INITIAL.
          response->set_text( get_html(   ) ).
        ELSE.
          DATA(name_filter) = xco_cp_abap_repository=>object_name->get_filter(
                               xco_cp_abap_sql=>constraint->contains_pattern( to_upper( sap_table_request ) && '%' )  ).
          DATA(objects) = xco_cp_abap_repository=>objects->tabl->where( VALUE #(
                              ( name_filter ) ) )->in( xco_cp_abap=>repository  )->get(  ).

          DATA(res) = `[`.
          LOOP AT objects INTO DATA(object).
            res &&= |\{ "TABLE_NAME": "{ object->name }" \}|.
            IF sy-tabix NE lines( objects ).
              res &&= `,`.
            ENDIF.
          ENDLOOP.
          res &&= `]`.
          response->set_text( res ).
        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).

* the request comes in with metadata around the actual file data,
* extract the filename and fileext from this metadata as well as the raw file data.
        SPLIT request->get_text(  )  AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(content).
        READ TABLE content REFERENCE INTO DATA(content_item) INDEX 2.
        IF sy-subrc = 0.

          SPLIT content_item->* AT ';' INTO TABLE DATA(content_dis).
          READ TABLE content_dis REFERENCE INTO DATA(content_dis_item) INDEX 3.
          IF sy-subrc = 0.
            SPLIT content_dis_item->* AT '=' INTO DATA(fn) filename.
            REPLACE ALL OCCURRENCES OF `"` IN filename WITH space.
            CONDENSE filename NO-GAPS.
            SPLIT filename AT '.' INTO filename fileext.
          ENDIF.

        ENDIF.

        DELETE content FROM 1 TO 4.  " Get rid of the first 4 lines
        DELETE content FROM ( lines( content ) - 8 ) TO lines( content ).  " get rid of the last 9 lines

 IF fileext EQ 'json'.
        LOOP AT content REFERENCE INTO content_item.  " put it all back together again humpdy dumpdy....
          filedata = filedata && content_item->*.
        ENDLOOP.
ELSEIF fileext EQ 'csv'.
LOOP AT content REFERENCE INTO content_item.  " put it all back together again humpdy dumpdy....
          IF content_item->* IS INITIAL OR content_item->* CS '----'.
          EXIT.
          ELSE.
          ls_file_cont-file_content = content_item->*.
          APPEND ls_file_cont TO lt_file_cont.
          ENDIF.
        ENDLOOP.
      ENDIF.
* Unpack input field values such as tablename, dataoption, etc.
        DATA(ui_data) = request->get_form_field(  `filetoupload-data` ).
        DATA(ui_dataref) = /ui2/cl_json=>generate( json = ui_data ).
        IF ui_dataref IS BOUND.
          ASSIGN ui_dataref->* TO FIELD-SYMBOL(<ui_dataref>).
          tablename = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref> ).
          dataoption = me->get_input_field_value( name = `DATAOPTION` dataref = <ui_dataref> ).
        ENDIF.

* Check table name is valid.
        IF xco_cp_abap_repository=>object->tabl->database_table->for(
                             iv_name =  CONV #( tablename ) )->exists(  ) = abap_false
          OR tablename IS INITIAL.
          response->set_status( i_code = if_web_http_status=>bad_request
                                i_reason = |Table name { tablename } not valid or does not exist| ).
          response->set_text( |Table name { tablename } not valid or does not exist| ).
          RETURN.
        ENDIF.

* Check file extension is valid, only json today.
        IF fileext <> `json`.
        IF fileext <> 'csv'.
          response->set_status( i_code = if_web_http_status=>bad_request
                                i_reason = `File type not supported` ).
          response->set_text( `File type not supported` ).
          RETURN.
        ENDIF.
        ENDIF.

* Load the data to the table via dynamic internal table
        DATA: dynamic_table TYPE REF TO data.
        FIELD-SYMBOLS: <table_structure> TYPE table.

        TRY.
            CREATE DATA dynamic_table TYPE TABLE OF (tablename).
            ASSIGN dynamic_table->* TO <table_structure>.
          CATCH cx_sy_create_data_error INTO DATA(cd_exception).
            response->set_status( i_code = if_web_http_status=>bad_request
                                 i_reason = cd_exception->get_text(  ) ).
            response->set_text( cd_exception->get_text(  )  ).
            RETURN.
        ENDTRY.
IF fileext EQ 'json'.
        /ui2/cl_json=>deserialize( EXPORTING json = filedata
                                   pretty_name = /ui2/cl_json=>pretty_mode-none
                                   CHANGING data = <table_structure> ).
 "Below is the addition of code for csv file
ELSEIF fileext EQ  'csv'.
  DATA: w_tab          TYPE STANDARD TABLE OF abap_compdescr,
        w_tab_wa       TYPE abap_compdescr,
        w_typ          TYPE REF TO cl_abap_elemdescr,
        lt_tot_comp    TYPE cl_abap_structdescr=>component_table,
        lt_comp        TYPE cl_abap_structdescr=>component_table,
        la_comp        LIKE LINE OF lt_comp,
        lo_new_type    TYPE REF TO cl_abap_structdescr,
        lo_table_type  TYPE REF TO cl_abap_tabledescr,
        w_tref         TYPE REF TO data,
        w_dy_line      TYPE REF TO data.

DATA: new_line TYPE REF TO data.
DATA: tab TYPE REF TO data.
CREATE DATA tab TYPE (tablename).
CREATE DATA new_line TYPE STANDARD TABLE OF (tablename).
  data : ref_table_des type ref to cl_abap_structdescr.
ref_table_des ?=
      cl_abap_typedescr=>describe_by_name( tablename ).
FIELD-SYMBOLS:
                 <struct> TYPE ANY,
                 <comp> TYPE ANY,
                 <table> TYPE any TABLE.

DATA: gv_table_name   TYPE string,
      gr_type_desc    TYPE REF TO cl_abap_typedescr,
      gr_struct_desc  TYPE REF TO cl_abap_structdescr,
      gr_table_desc   TYPE REF TO cl_abap_tabledescr,
      gv_t            TYPE c,
      gv_comp         TYPE i,
      gr_table_ref    TYPE REF TO data,
      gr_struc_ref   TYPE REF TO data.

DATA: gt_itab   TYPE TABLE OF string,
      gt_split  TYPE TABLE OF string,
      gv_str    TYPE string.


DATA(lt_table) = ref_table_des->components[].

cl_abap_tabledescr=>describe_by_name(
      EXPORTING p_name = tablename
      RECEIVING p_descr_ref = gr_type_desc
      EXCEPTIONS type_not_found = 4 ).
gr_struct_desc ?= gr_type_desc.
gr_table_desc = cl_abap_tabledescr=>create( gr_struct_desc ).
CREATE DATA gr_table_ref TYPE HANDLE gr_table_desc.
CREATE DATA gr_struc_ref TYPE HANDLE gr_struct_desc.
ASSIGN gr_table_ref->* TO <table>.
ASSIGN gr_struc_ref->* TO <struct>.

LOOP AT lt_table INTO DATA(ls_table).
  w_tab_wa-name      = ls_table-name.
  w_tab_wa-type_kind = ls_table-type_kind.         "Char field
  w_tab_wa-length    = ls_table-length.
  w_tab_wa-decimals = ls_table-decimals.
  APPEND w_tab_wa TO w_tab.
ENDLOOP.

DATA(lv_count) = LINES( lt_table ).

LOOP AT lt_file_cont INTO ls_file_cont.
SPLIT ls_file_cont-file_content AT ',' INTO TABLE gt_split.
DO lv_count TIMES.
READ TABLE gt_split INTO gv_str INDEX sy-index.
ASSIGN COMPONENT sy-index OF STRUCTURE <struct> TO <comp>.
<comp> = gv_str.
CLEAR: gv_str.
ENDDO.
INSERT <struct> INTO TABLE <table_structure>.
ENDLOOP.
ENDIF.
        IF dataoption = `1`.  "if replace, delete the data from the table first
          DELETE FROM (tablename).
        ENDIF.

        TRY.
            INSERT (tablename) FROM TABLE @<table_structure>.
            IF sy-subrc = 0.
              response->set_status( i_code = if_web_http_status=>ok
                                    i_reason = `Table updated successfully` ).
              response->set_text( `Table updated successfully` ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO DATA(db_exception).
            response->set_status( i_code = if_web_http_status=>bad_request
                                 i_reason = db_exception->get_text(  ) ).
            response->set_text( db_exception->get_text(  )  ).
            RETURN.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.

  METHOD get_input_field_value.

    FIELD-SYMBOLS: <value> TYPE data,
                   <field> TYPE any.

    ASSIGN COMPONENT name  OF STRUCTURE dataref TO <field>.
    IF <field> IS ASSIGNED.
      ASSIGN <field>->* TO <value>.
      value = condense( <value> ).
    ENDIF.

  ENDMETHOD.

  METHOD get_html.
    ui_html =
    |<!DOCTYPE HTML> \n| &&
     |<html> \n| &&
     |<head> \n| &&
     |    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \n| &&
     |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \n| &&
     |    <title>ABAP File Uploader</title> \n| &&
     |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
     |        data-sap-ui-theme="sap_fiori_3_dark" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
     |        data-sap-ui-async="true"> \n| &&
     |    </script> \n| &&
     |    <script> \n| &&
     |        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \n| &&
     | \n| &&
     |            sap.ui.getCore().loadLibrary("sap.f", \{ \n| &&
     |                async: true \n| &&
     |            \}).then(() => \{ \n| &&
     |                let shell = new sap.f.ShellBar("shell") \n| &&
     |                shell.setTitle("ABAP File Uploader") \n| &&
     |                shell.setShowCopilot(true) \n| &&
     |                shell.setShowSearch(true) \n| &&
     |                shell.setShowNotifications(true) \n| &&
     |                shell.setShowProductSwitcher(true) \n| &&
     |                shell.placeAt("uiArea") \n| &&
     |                sap.ui.getCore().loadLibrary("sap.ui.layout", \{ \n| &&
     |                    async: true \n| &&
     |                \}).then(() => \{ \n| &&
     |                    let layout = new sap.ui.layout.VerticalLayout("layout") \n| &&
     |                    layout.placeAt("uiArea") \n| &&
     |                    let line2 = new sap.ui.layout.HorizontalLayout("line2") \n| &&
     |                    let line3 = new sap.ui.layout.HorizontalLayout("line3") \n| &&
     |                    let line4 = new sap.ui.layout.HorizontalLayout("line4") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.m", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{\}) \n| &&
     |                    let button = new sap.m.Button("button") \n| &&
     |                    button.setText("Upload File") \n| &&
     |                    button.attachPress(function () \{ \n| &&
     |                        let oFileUploader = oCore.byId("fileToUpload") \n| &&
     |                        if (!oFileUploader.getValue()) \{ \n| &&
     |                            sap.m.MessageToast.show("Choose a file first") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                        let oInput = oCore.byId("tablename") \n| &&
     |                        let oGroup = oCore.byId("grpDataOptions") \n| &&
     |                        if (!oInput.getValue())\{ \n| &&
     |                            sap.m.MessageToast.show("Target Table is Required") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                       let param = oCore.byId("uploadParam") \n| &&
     |                       param.setValue( oInput.getValue() ) \n| &&
     |                       oFileUploader.getParameters() \n| &&
     |                       oFileUploader.setAdditionalData(JSON.stringify(\{tablename: oInput.getValue(), \n| &&
     |                           dataOption: oGroup.getSelectedIndex() \})) \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    let input = new sap.m.Input("tablename") \n| &&
     |                    input.placeAt("layout") \n| &&
     |                    input.setRequired(true) \n| &&
     |                    input.setWidth("600px") \n| &&
     |                    input.setPlaceholder("Target ABAP Table") \n| &&
     |                    input.setShowSuggestion(true) \n| &&
     |                    input.attachSuggest(function (oEvent)\{ \n| &&
     |                      jQuery.ajax(\{headers: \{ "sap-table-request": oEvent.getParameter("suggestValue") \n | &&
     |                          \}, \n| &&
     |                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 30000, method:"GET",dataType: "json",success: function(myJSON) \{ \n| &&
 "   |                      alert( 'test' ) \n| &&
     |                      let input = oCore.byId("tablename") \n | &&
     |                      input.destroySuggestionItems() \n | &&
     |                      for (var i = 0; i < myJSON.length; i++) \{ \n | &&
     |                          input.addSuggestionItem(new sap.ui.core.Item(\{ \n| &&
     |                              text: myJSON[i].TABLE_NAME  \n| &&
     |                          \})); \n| &&
     |                      \} \n| &&
     |                    \} \}) \n| &&
     |                    \}) \n| &&
     |                    line2.placeAt("layout") \n| &&
     |                    line3.placeAt("layout") \n| &&
     |                    line4.placeAt("layout") \n| &&
     |                    let groupDataOptions = new sap.m.RadioButtonGroup("grpDataOptions") \n| &&
     |                    let lblGroupDataOptions = new sap.m.Label("lblDataOptions") \n| &&
     |                    lblGroupDataOptions.setLabelFor(groupDataOptions) \n| &&
     |                    lblGroupDataOptions.setText("Data Upload Options") \n| &&
     |                    lblGroupDataOptions.placeAt("line3") \n| &&
     |                    groupDataOptions.placeAt("line4") \n| &&
     |                    rbAppend = new sap.m.RadioButton("rbAppend") \n| &&
     |                    rbReplace = new sap.m.RadioButton("rbReplace") \n| &&
     |                    rbAppend.setText("Append") \n| &&
     |                    rbReplace.setText("Replace") \n| &&
     |                    groupDataOptions.addButton(rbAppend) \n| &&
     |                    groupDataOptions.addButton(rbReplace) \n| &&
     |                    rbAppend.setGroupName("grpDataOptions") \n| &&
     |                    rbReplace.setGroupName("grpDataOptions") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.ui.unified", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{ \n| &&
     |                        var fileUploader = new sap.ui.unified.FileUploader( \n| &&
     |                            "fileToUpload") \n| &&
     |                        fileUploader.setFileType("json") \n| &&
     |                        fileUploader.setWidth("400px") \n| &&
     |                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \n| &&
     |                        param.setName("tablename") \n| &&
     |                        fileUploader.addParameter(param) \n| &&
     |                        fileUploader.placeAt("line2") \n| &&
     |                        button.placeAt("line2") \n| &&
     |                        fileUploader.setPlaceholder( \n| &&
     |                            "Choose File for Upload...") \n| &&
     |                        fileUploader.attachUploadComplete(function (oEvent) \{ \n| &&
     |                           alert(oEvent.getParameters().response)  \n| &&
     |                       \})   \n| &&
     | \n| &&
     |                    \}) \n| &&
     |                \}) \n| &&
     |            \}) \n| &&
     |        \}) \n| &&
     |    </script> \n| &&
     |</head> \n| &&
     |<body class="sapUiBody"> \n| &&
     |    <div id="uiArea"></div> \n| &&
     |</body> \n| &&
     | \n| &&
     |</html> |.

  endmethod.
  METHOD get_html_csv.
    ui_html =
    |<!DOCTYPE HTML> \n| &&
     |<html> \n| &&
     |<head> \n| &&
     |    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \n| &&
     |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \n| &&
     |    <title>Test ABAP File Uploader</title> \n| &&
     |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
     |        data-sap-ui-theme="sap_belize" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
     |        data-sap-ui-async="true"> \n| &&
     |    </script> \n| &&
     |    <script> \n| &&
     |        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \n| &&
     | \n| &&
     |            sap.ui.getCore().loadLibrary("sap.f", \{ \n| &&
     |                async: true \n| &&
     |            \}).then(() => \{ \n| &&
     |                let shell = new sap.f.ShellBar("shell") \n| &&
     |                shell.setTitle("Test ABAP File Uploader") \n| &&
     |                shell.setShowCopilot(false) \n| &&
     |                shell.setShowSearch(false) \n| &&
     |                shell.setShowNotifications(false) \n| &&
     |                shell.setShowProductSwitcher(false) \n| &&
     |                shell.placeAt("uiArea") \n| &&
     |                sap.ui.getCore().loadLibrary("sap.ui.layout", \{ \n| &&
     |                    async: true \n| &&
     |                \}).then(() => \{ \n| &&
     |                    let layout = new sap.ui.layout.VerticalLayout("layout") \n| &&
     |                    layout.placeAt("uiArea") \n| &&
     |                    let line2 = new sap.ui.layout.HorizontalLayout("line2") \n| &&
     |                    let line3 = new sap.ui.layout.HorizontalLayout("line3") \n| &&
     |                    let line4 = new sap.ui.layout.HorizontalLayout("line4") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.m", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{\}) \n| &&
     |                    let button = new sap.m.Button("button") \n| &&
     |                    button.setText("Upload File") \n| &&
     |                    button.attachPress(function () \{ \n| &&
     |                        let oFileUploader = oCore.byId("fileToUpload") \n| &&
     |                        if (!oFileUploader.getValue()) \{ \n| &&
     |                            sap.m.MessageToast.show("Choose a file first") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                        let oInput = oCore.byId("tablename") \n| &&
     |                        let oGroup = oCore.byId("grpDataOptions") \n| &&
     |                        if (!oInput.getValue())\{ \n| &&
     |                            sap.m.MessageToast.show("Target Table is Required") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                       let param = oCore.byId("uploadParam") \n| &&
     |                       param.setValue( oInput.getValue() ) \n| &&
     |                       oFileUploader.getParameters() \n| &&
     |                       oFileUploader.setAdditionalData(JSON.stringify(\{tablename: oInput.getValue(), \n| &&
     |                           dataOption: oGroup.getSelectedIndex() \})) \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    let input = new sap.m.Input("tablename") \n| &&
     |                    input.placeAt("layout") \n| &&
     |                    input.setRequired(true) \n| &&
     |                    input.setWidth("600px") \n| &&
     |                    input.setPlaceholder("Target ABAP Table") \n| &&
     |                    input.setShowSuggestion(true) \n| &&
     |                    input.attachSuggest(function (oEvent)\{ \n| &&
     |                      jQuery.ajax(\{headers: \{ "sap-table-request": oEvent.getParameter("suggestValue") \n | &&
     |                          \}, \n| &&
     |                         error: function(oErr)\{ alert( JSON.stringify(oErr))\}, timeout: 30000, method:"GET",dataType: "json",success: function(myJSON) \{ \n| &&
 "   |                      alert( 'test' ) \n| &&
     |                      let input = oCore.byId("tablename") \n | &&
     |                      input.destroySuggestionItems() \n | &&
     |                      for (var i = 0; i < myJSON.length; i++) \{ \n | &&
     |                          input.addSuggestionItem(new sap.ui.core.Item(\{ \n| &&
     |                              text: myJSON[i].TABLE_NAME  \n| &&
     |                          \})); \n| &&
     |                      \} \n| &&
     |                    \} \}) \n| &&
     |                    \}) \n| &&
     |                    line2.placeAt("layout") \n| &&
     |                    line3.placeAt("layout") \n| &&
     |                    line4.placeAt("layout") \n| &&
     |                    let groupDataOptions = new sap.m.RadioButtonGroup("grpDataOptions") \n| &&
     |                    let lblGroupDataOptions = new sap.m.Label("lblDataOptions") \n| &&
     |                    lblGroupDataOptions.setLabelFor(groupDataOptions) \n| &&
     |                    lblGroupDataOptions.setText("Data Upload Options") \n| &&
     |                    lblGroupDataOptions.placeAt("line3") \n| &&
     |                    groupDataOptions.placeAt("line4") \n| &&
     |                    rbAppend = new sap.m.RadioButton("rbAppend") \n| &&
     |                    rbReplace = new sap.m.RadioButton("rbReplace") \n| &&
     |                    rbAppend.setText("Append") \n| &&
     |                    rbReplace.setText("Replace") \n| &&
     |                    groupDataOptions.addButton(rbAppend) \n| &&
     |                    groupDataOptions.addButton(rbReplace) \n| &&
     |                    rbAppend.setGroupName("grpDataOptions") \n| &&
     |                    rbReplace.setGroupName("grpDataOptions") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.ui.unified", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{ \n| &&
     |                        var fileUploader = new sap.ui.unified.FileUploader( \n| &&
     |                            "fileToUpload") \n| &&
     |                        fileUploader.setFileType["csv","json"] \n| &&
     |                        fileUploader.setWidth("400px") \n| &&
     |                        let param = new sap.ui.unified.FileUploaderParameter("uploadParam") \n| &&
     |                        param.setName("tablename") \n| &&
     |                        fileUploader.addParameter(param) \n| &&
     |                        fileUploader.placeAt("line2") \n| &&
     |                        button.placeAt("line2") \n| &&
     |                        fileUploader.setPlaceholder( \n| &&
     |                            "Choose File for Upload...") \n| &&
     |                        fileUploader.attachUploadComplete(function (oEvent) \{ \n| &&
     |                           alert(oEvent.getParameters().response)  \n| &&
     |                       \})   \n| &&
     | \n| &&
     |                    \}) \n| &&
     |                \}) \n| &&
     |            \}) \n| &&
     |        \}) \n| &&
     |    </script> \n| &&
     |</head> \n| &&
     |<body class="sapUiBody"> \n| &&
     |    <div id="uiArea"></div> \n| &&
     |</body> \n| &&
     | \n| &&
     |</html> |.
  ENDMETHOD.
ENDCLASS.

