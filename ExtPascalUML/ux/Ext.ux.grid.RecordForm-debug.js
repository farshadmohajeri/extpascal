// vim: sw=4:ts=4:nu:nospell:fdc=4
/**
 * Ext.ux.grid.RecordForm Form that edits grid's record
 *
 * @author    Ing. Jozef Sakáloš
 * @copyright (c) 2008, by Ing. Jozef Sakáloš
 * @date      4. April 2008
 * @version   $Id: Ext.ux.grid.RecordForm.js 317 2008-08-21 21:31:51Z jozo $
 *
 * @license Ext.ux.grid.RecordForm.js is licensed under the terms of the Open Source
 * LGPL 3.0 license. Commercial use is permitted to the extent that the 
 * code/component(s) do NOT become part of another Open Source or Commercially
 * licensed development library or toolkit without explicit permission.
 * 
 * License details: http://www.gnu.org/licenses/lgpl.html
 */
 
/*global Ext */
 
Ext.ns('Ext.ux.grid');

/**
 * @class Ext.ux.grid.RowActions
 * @extends Ext.util.Observable
 *
 * Creates new RowActions plugin
 * @constructor
 * @param {Object} config The config object
 */
Ext.ux.grid.RecordForm = function(config) {
	Ext.apply(this, config);

	// call parent
	Ext.ux.grid.RecordForm.superclass.constructor.call(this);
}; // eo constructor

Ext.extend(Ext.ux.grid.RecordForm, Ext.util.Observable, {

	// {{{
	// configuration options
	 autoHide:true
	/**
	 * @cfg {String} cancelIconCls Icon class for cancel button
	 */
	 ,cancelIconCls:'icon-cancel'
	
	,grid:null

	/**
	 * @cfg {String} cancelText Text for cancel button
	 */
	,cancelText:'Cancel'

	/**
	 * @cfg {Number} columnCount Form fields are arranged into columns; 
	 * this says how many columns you want
	 */
	,columnCount:1
	/**
	 * @cfg {Object} defaultFormConfig Default configuration of form
	 * @private
	 */
	,defaultFormConfig: {
		 border:false
		,frame:true
            ,width:480
		,autoHeight:true
		,labelWidth:100
		,buttonAlign:'right'
		,bodyStyle:'padding-top:10px'
	}

	/**
	 * @cfg {Object} defaultWindowConfig Default configuration of widnow
	 * @private
	 */
	,defaultWindowConfig:{
		 border:false
		,autoHeight:true
		,layout:'fit'
		,closeAction:'hide'
		,modal:true
// Wanderlan ,plugins:[new Ext.ux.IconMenu({defaultItems:[]})]
	}

	/** 
	 * @cfg {String} dirtyRowCls class to apply to dirty (edited) row
	 */
	,dirtyRowCls:'ux-grid3-dirty-row'

	/**
	 * @cfg {Number} focusDefer Time in ms before the first form field is focused
	 * @private
	 */
	,focusDefer:200

	/**
	 * @cfg {Object} formConfig Additional configuration options for form
	 * Overrides defaultFormConfig
	 */

	/**
	 * @cfg {String} iconCls icon to use for title of the popup window
	 */

	/**
	 * @cfg {Object} ignoreFields Object {fieldName:true, fieldName2:true} with
	 * fields to ignore. These fields are not displayed in the form.
	 */

	/**
	 * @cfg {Object} readonlyFields Object {fieldName:true, fieldName2:true} with
	 * fields to set as read only.
	 */

	/**
	 * @cfg {Object} disabledFields Object {fieldName:true, fieldName2:true} with
	 * fields to set as disabled.
	 */

	/**
	 * @cfg {Object} mapping Mapping of Record types to Field xtypes
	 */
	,mapping:{
		 'auto':'textfield'
		,'boolean':'checkbox'
		,'date':'datefield'
		,'float':'numberfield'
		,'int':'numberfield'
		,'string':'textfield'
	}

	/**
	 * @cfg {String} newRowCls class to apply to new row
	 */
	,newRowCls:'ux-grid3-new-row'

	/**
	 * @cfg {String} okIconCls Icon class for OK button
	 */
	,okIconCls:'icon-ok'

	/**
	 * @cfg {String} okText Text for OK button
	 */
	,okText:'OK'

	/**
	 * @cfg {String} title Title to use for popup window
	 */
	
	/**
	 * @cfg {Boolean} showButtons false to not show OK and Cancel buttons. (defaults to true)
	 */
	,showButtons:true

	/**
	 * @cfg {Object} windowConfig Additional configuration options for window.
	 * Overrides defaultWindowConfig.
	 */
	// }}}
	// {{{
	/**
	 * Main init function
	 * @private
	 */
	,init:function(grid) {

		// install custom getRowClass to grid view
		grid.afterRender = grid.afterRender.createSequence(function() {
			if('function' === typeof grid.view.getRowClass) {
				grid.view.getRowClass = grid.view.getRowClass.createSequence(this.getRowClass, this);
			}
			else {
				grid.view.getRowClass = this.getRowClass.createDelegate(this);
			}
			if(this.autoShow) {
				this.show({data:{}});
			}
		}, this);

		// save reference to grid
		this.grid = grid; 

		// we need to reconfigure ourselves when grid reconfigures
		grid.reconfigure = grid.reconfigure.createSequence(this.reconfigure, this);

		// initial (re)configuration
		this.reconfigure();
	} // eo function init
	// }}}
	// {{{
	/**
	 * Override this to add processing you need to run after the record update
	 * @param {Ext.data.Record} record Record that has been updated
	 */
	,afterUpdateRecord:Ext.emptyFn
	// }}}
	// {{{
	/**
	 * Creates form configuration. Form is created later in show function
	 * @private
	 */
	,createFormConfig:function() {

		// run only once 
		if(this.form) {
			return;
		}

		// get vars
		var cm = this.grid.getColumnModel();
		var fields = this.grid.store.recordType.prototype.fields;
		var store = this.grid.store;

		// {{{
		// create form *config* object - it does NOT instantiate the form
		this.form = Ext.apply({
			 xtype:'form'
			,items:[{

				// column layout
				 layout:'column'
				,anchor:'100%'
				,border:false
				,monitorValid:true
				,autoHeight:true
				,defaults:{
					 columnWidth:1/this.columnCount
					,autoHeight:true
					,border:false
					,layout:'form'
					,hideLabel:true
				}

				// columns
				,items:(function() {
					var items = [];
					for(var i = 0; i < this.columnCount; i++) {
						items.push({
							defaults:{
								anchor:'-8' // Wanderlan
							}
							,items:[]
						});
					}
					return items;
				}).createDelegate(this)()
			}]

			// buttons
			,buttons:(function() {
				if(this.showButtons) {
					return [{
						 text:this.okText
						,iconCls:this.okIconCls
						,scope:this
						,handler:this.onOK
						,formBind:true
					},{
						 text:this.cancelText
						,iconCls:this.cancelIconCls
						,scope:this
						,handler:this.onCancel
					}];
				}
				else {
					return [];
				}
			}).createDelegate(this)()

			// ok on enter
			,keys:[{
				 key:[10,13] // enter
				,scope:this
				,stopEvent:true
				,fn:this.onOK
			}]
		}, this.formConfig, this.defaultFormConfig); // eo form config
		// }}}
		// {{{
		// add form fields from store or column model. cm has priority
		var colIndex = 0;
		var tabIndex = 1;

		// store record fields loop
		fields.each(function(f, i) {
			// ignore some fields
			if(this.ignoreFields && this.ignoreFields[f.name]) {
				return;
			}

			// attempt to get config from column model
			var c = this.findConfig(cm, f.name);
			var o = {};

			// use cm editor if we have one
			if(c && c.editor && c.editor.field) {
				Ext.apply(o, {
					 xtype:c.editor.field.getXType()
					,fieldLabel:c.header
				}, c.editor.field.initialConfig);
			}

			// use this.mapping to get field xtype
			else {
				Ext.apply(o, {
					 fieldLabel:(c && c.header ? c.header : f.name)
					,xtype:this.mapping[f.type] || 'textfield'
				});
				if('date' === f.type && f.dateFormat) {
					o.format = f.dateFormat;
				}
			}

			// read only and disabled fields
			if(this.readonlyFields && true === this.readonlyFields[f.name]) {
				o.readOnly = true;
			}
			if(this.disabledFields && true === this.disabledFields[f.name]) {
				o.disabled = true;
			}
			
			// field has to have name
			o.name = f.name;
			o.tabIndex = tabIndex++;

			// do not anchor date and time fields
			if('datefield' === o.xtype || 'timefield' === o.xtype || 'datetime' === o.xtype) {
				o.anchor = '';
			}
			if('textarea' === o.xtype) {
				o.grow = false;
				o.autoHeight = true;
			}

			// add field to a column on left-to-right top-to-bottom basis
			this.form.items[0].items[colIndex++].items.push(o);
			colIndex = colIndex === this.columnCount ? 0 : colIndex;
			
		}, this);
		// }}}

	} // eo function createFormConfig
	// }}}
	// {{{
	/**
	 * Finds if a configuration exists for a given dataIndex in column model
	 * @private
	 * @param {Ext.grid.ColumnModel} cm
	 * @param {String} dataIndex 
	 */
	,findConfig:function(cm, dataIndex) {
		var config = null;
		Ext.each(cm.config, function(c, i) {
			if(config) {
				return;
			}
			if(dataIndex === c.dataIndex) {
				config = c;
			}
		});

		return config;
	} // eo function findConfig
	// }}}
	// {{{
	/**
	 * GridVew getRowClass sequence function - override it to get custom effects
	 * @param {Ext.data.Record} record record we should return the class for
	 */
	,getRowClass:function(record) {
		if(record.get('newRecord')) {
			return this.newRowCls;
		}
		if(record.dirty) {
			return this.dirtyRowCls;
		}
		return '';
	} // eo function getRowClass
	// }}}
	// {{{
	/**
	 * Destroys components we've created
	 * @private
	 */
	,onDestroy:function() {
		if(this.window) {
			this.window.destroy();
			this.window = null;
			this.form = null;
		}
		else if(this.form) {
			if('function' === typeof this.form.destroy) {
				this.form.destroy();
			}
			this.form = null;
		}
	} // eo function onDestroy
	// }}}
	// {{{
	/**
	 * OK button click handler
	 */
	,onOK:function() {
		this.updateRecord();
		if(this.autoHide) {
			this.window.hide(null);
		}
	} // eo function onOK
	// }}}
	// {{{
	/**
	 * Cancel button handler, removes new record if it is not dirty
	 */
	,onCancel:function() {
		if(this.record.get('newRecord') && !this.record.dirty) {
			this.record.store.remove(this.record);
		}
		if(this.autoHide) {
			this.window.hide(null);
		}
	} // eo function onCancel
	// }}}
	// {{{
	/**
	 * Reconfigures the plugin - deletes old form and creates new one
	 * Runs also after grid reconfigure call
	 * @private
	 */
	,reconfigure:function() {
		// destroy old window and form
		this.onDestroy();

		// create new form configuration
		// form will be instantiated and rendered in show function
		this.createFormConfig();
	} // eo function reconfigure
	// }}}

	,getPanel:function() {
		if(this.window) {
			return this.window;
		}
		if(this.formCt) {
			var panel = Ext.getCmp(this.formCt);
			if(panel) {
				panel.add(this.form);
				panel.doLayout();
			}
			else {
				panel = Ext.fly(this.formCt);
				if(panel) {
					panel = new Ext.Panel({
						 renderTo:panel
						,items:this.form
					});
				}
			}
		}
		else {
			var config = Ext.apply({}, this.defaultWindowConfig);
			config = Ext.apply(config, this.windowConfig);
			Ext.applyIf(config, {
				 title:this.title || this.grid.title
				,iconCls:this.iconCls || this.grid.iconCls
				,items:this.form
				,listeners:{
					show:{scope:this, delay:this.focusDefer, fn:function() {
						this.form.startMonitoring();
						var basicForm = this.form.getForm()

						// focus first form field on window show
						basicForm.items.itemAt(0).focus();

						// mark fields invalid if any
						basicForm.isValid();
					}}
					,hide:{scope:this, fn:function() {
						this.form.stopMonitoring();
					}}
				}
			});
			var window = new Ext.Window(config);
			this.form = window.items.itemAt(0);
			return window;
		}
		panel.on({
			show:{scope:this, delay:this.focusDefer, fn:function() {
				this.form.startMonitoring();
				var basicForm = this.form.getForm()

				// focus first form field on window show
				basicForm.items.itemAt(0).focus();

				// mark fields invalid if any
				basicForm.isValid();
			}}
			,hide:{scope:this, fn:function() {
				this.form.stopMonitoring();
			}}
		});
		this.form = panel.items.itemAt(0);
		return panel;

	} // eo function getPanel

	// {{{
	/**
	 * Shows the record form in the window
	 * @param {Ext.data.Record} record Record to bind to
	 * @param {Ext.Element/DOMElement/String} animEl window show animation element
	 */
	,show:function(record, animEl) {


		// lazy create window
		if(!this.window) {
			this.window = this.getPanel();
		}

		// show window
		this.window.show(animEl);

		// populate fields with values
		var basicForm = this.form.getForm();
		basicForm.loadRecord(record);


		// save record we're currently editing
		this.record = record;
	} // eo function show
	// }}}
	// {{{
	/**
	 * Updates record in store
	 * @private
	 */
	,updateRecord:function() {
		// loop through form fields and update underlying record
		this.form.getForm().items.each(function(item, i) {
			this.record.set(item.name, item.getValue());
		}, this);

		this.afterUpdateRecord(this.record);

	} // eo function updateRecord
	// }}}
 
}); // eo extend

// register xtype
Ext.reg('gridrecordform', Ext.ux.grid.RecordForm); 
 
// eof
