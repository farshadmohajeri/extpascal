/*
 * CodePress - Real Time Syntax Highlighting Editor written in JavaScript - http://codepress.org/
 *
 * Copyright (C) 2006 Fernando M.A.d.S. <fermads@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation.
 *
 * Read the full licence: http://www.opensource.org/licenses/lgpl-license.php
 *
 * This file is not the original but has been modified to be more compliant with
 * ExtJs. Changes are made by S.J.Hoeksma
 *
 * Adapted to Object Pascal by Wanderlan Santos dos Anjos
 */
Ext.namespace('Ext.ux');

/**
 * Component which wraps the <a href="http://codepress.org">CodePress</a> library to make
 * it available for ExtJs. CodePress gives syntax highlighting for different programming
 * languages.
 * @type component
 */
Ext.ux.CodePress = Ext.extend(Ext.form.Field, {

    /**
     * The id of the element to pull code from
     * @type {String}
     @cfg */
    sourceEl : false,

    /**
     * The code to use in the editor
     * @type {String}
     @cfg */
     code : false,

    /**
     * The language to render the code with (defaults 'pascal')
     * @type {String}
     @cfg */
    language : 'pascal',

    /**
     * The url used to read code which is display in editor
     * @type {String}
     @cfg  */
    url : false,

    /**
     * Height of the editor (defaults false)
     * @type {Int}
     @cfg  */
    height : false,

    /**
     * Width of the editor (defaults false)
     * @type {Int}
     @cfg */
    width : false,

    /**
     * AutoResize window on change container (defaults true)
     * @type {Boolean}
     @cfg */
    autoResize : true,

    /**
     * Trim the code of trailing spaces and empty lines (defaults true)
     * @type {Boolean}
     @cfg */
    trim  : true,

    /**
     * Is autoComplete for keywords turned on or off (defaults true)
     * @type {Boolean}
     @cfg */
    autoComplete : true,

    /**
     * Is the editor readonly (defaults false)
     * @type {Boolean}
     @cfg */
    readOnly  : false,

    /**
     * Are lineNumbers visible (defaults true)
     * @type {Boolean}
     @cfg */
    lineNumbers : true,

    //@private Has the editor been initialized
    initialized : false,

    /**
     * Path where codepress is located
     * @type {String}
     @cfg */
    path : undefined,

    /**
     * @private Init the codepress component for ExtJs
     */
    initComponent : function(){
      if (!Ext.ux.CodePress.path) {
         s = document.getElementsByTagName('script');
         for(var i=0,n=s.length;i<n;i++) {
           var name = s[i].src ? s[i].src : s[i].id;
           if(name.match('Ext\.ux\.CodePress\-debug\.js')) {
             Ext.ux.CodePress.path = name.replace("Ext.ux.CodePress-debug.js",'');
            break;
           }
         }
      }
      if (this.path==undefined) this.path = Ext.ux.CodePress.path || "";
      if (this.path && this.path.charAt(this.path.length-1)!='/') this.path += '/';
      Ext.ux.CodePress.superclass.initComponent.call(this);

      // Hide the sourceEl if provided
      if(this.sourceEl) Ext.get(this.sourceEl).hide();

      this.addEvents({
          /**
           * Fires when the editor is fully initialized (including the iframe)
           * @event initialize
           * @param {Object} editor The editor
           */
          initialize: true,

          /**
           * Fires when the editor is first receives the focus. Any insertion must wait
           * until after this event.
           * @event activate
           * @param {Object} editor The editor when activated
           */
          activate: true

      });
    },

   /**
    * @private (for BoxComponent)
    */
   adjustSize : Ext.BoxComponent.prototype.adjustSize,

    /**
     * Resize the the editor depending, behavior depends on height,width and autoResize
     */
   resize : function(){
      if (!this.editor) return;
      var h,w;
      if (this.autoResize) {
        h =  this.ownerCt.body.dom.clientHeight +'px';
        w =  this.ownerCt.body.dom.clientWidth +'px';
      } else {
       h = (this.height || this.ownerCt.body.dom.clientHeight) +'px';
       w = (this.width || this.ownerCt.body.dom.clientWidth) +'px';
      }
      this.editor.body.style.width = w;
      this.iframe.setStyle('height', h);
      this.iframe.setStyle('width', w);
    },

    /**
     * @private During render we create textarea of code press
     * @param {Component} ct The component to render
     * @param {Object} position A object containing the position of the component
     */
    onRender : function(ct, position){
        Ext.ux.CodePress.superclass.onRender.call(this, ct, position);

        //Taken from Ext.form.HtmlEditor
        this.el.dom.style.border = '0 none';
        this.el.dom.setAttribute('tabIndex', -1);
        this.el.addClass('x-hidden');


        if(Ext.isIE){ // fix IE 1px bogus margin
            this.el.applyStyles('margin-top:-1px;margin-bottom:-1px;')
        }
        this.wrap = this.el.wrap({});

        // Create the iframe
        this.iframe = Ext.get(document.createElement('iframe'));
        this.iframe.src = (Ext.SSL_SECURE_URL || 'javascript:false');

        // Create the textarea element if not created
        if(!this.sourceEl){
            this.textarea = Ext.get(document.createElement('textarea'));
          }else{
            this.textarea = Ext.get(this.sourceEl);
          }
        this.textarea.dom.disabled = true;
        this.textarea.dom.style.overflow = 'hidden';
        this.textarea.dom.style.overflow = 'auto';
        this.iframe.dom.frameBorder = 0; // remove IE internal iframe border
        this.iframe.setStyle('visibility', 'hidden');
        this.iframe.setStyle('position', 'absolute');
        this.options = this.textarea.dom.className;
        this.wrap.dom.appendChild(this.textarea.dom);
        this.textarea.dom.parentNode.insertBefore(this.iframe.dom, this.textarea.dom);
        this.edit();
        this.height = (this.height || this.ownerCt.body.dom.clientHeight);
        this.width= (this.width || this.ownerCt.body.dom.clientWidth);
    },

   /**
    * @private We don't support focus of editor
    */
    focus : function(){},

   /**
    * @private Initialize the editor
    */
   initialize : function() {
      if(Ext.isIE){
        this.doc = this.iframe.dom.contentWindow.document;
        this.win = this.iframe.dom.contentWindow;
      } else {
        this.doc = this.iframe.dom.contentDocument;
        this.win = this.iframe.dom.contentWindow;
      }
      this.editor = this.win.CodePress;
      this.editor.body = this.doc.getElementsByTagName('body')[0];
      if(this.url){
        Ext.Ajax.request({
          url: this.url
          , method:'get'
          , success:function(response, options){
            var code = response.responseText;
            this.code = code;
            this.editor.setCode(this.code);
            this.editor.syntaxHighlight('init'); // Wanderlan fix
          }.createDelegate(this)
        });
      }else{
        this.editor.setCode(this.code || this.textarea.dom.value);
      }
      this.resize();
      this.setOptions();
      this.editor.syntaxHighlight('init');
      this.textarea.dom.style.display = 'none';
      this.iframe.dom.style.position = 'static';
      this.iframe.dom.style.visibility = 'visible';
      this.iframe.dom.style.display = 'inline';

      this.initialized = true;
      this.fireEvent('initialize', this);
   },

   /**
    * Initailize the editor with a element and set the langauge
    * @param {Object} obj Can by a textarea id or a string
    * @param {String} language The langauge to use
    */
   edit : function(obj,language) {
    if(obj) this.textarea.dom.value = document.getElementById(obj) ? document.getElementById(obj).value : obj;
    if(!this.textarea.dom.disabled) return;
    this.language = language ? language : this.getLanguage();
    this.iframe.dom.src = this.path+'codepress.html?language='+this.language+'&ts='+(new Date).getTime();
    this.iframe.removeListener('load', this.initialize);
    this.iframe.on('load', this.initialize, this);
  },

  /**
   * Get the current langauge used by the editor
   * @return {String} The language used by editor
   */
  getLanguage : function() {
    if(this.language) return this.language;
    for (language in Ext.ux.CodePress.languages)
      if(this.options.match('\\b'+language+'\\b'))
        return Ext.ux.CodePress.languages[language] ? language : 'generic';
  },

  /**
   * Set the options of editor
   * See config items autoComplete, readOnly, lineNumbers
   */
  setOptions : function() {
    if(this.autoComplete===false || this.options.match('autocomplete-off')) this.toggleAutoComplete();
    if(this.readOnly===true || this.options.match('readonly-on')) this.toggleReadOnly();
    if(this.lineNumbers===false || this.options.match('linenumbers-off')) this.toggleLineNumbers();
  },

  /**
   * Original CodePress function to get the code from the editor. For compatibility reasons
   * with ExtJs TextArea whe implemented getValue
   * @return {String} The code from editor
   */
  getCode : function() {
    var code;
    if (this.textarea && this.editor)
     code = this.textarea.dom.disabled ? this.editor.getCode() : this.textarea.dom.value;
    else
     code = this.code || "";
    code =  this.trim ? code.replace(/^\s+|(\s+$|\n$|\r$)/g,"") : code;
    return code;
  },

  /**
   * Original CodePress function to set the code of the editor.For compatibility reasons
   * with ExtJs TextArea whe implemented setValue
   * @param {String} code The code to be display in editor
   */
  setCode : function(code) {
    if (this.textarea && this.editor) {
      this.textarea.dom.disabled ? this.editor.setCode(code) : this.textarea.dom.value = code;
      this.editor.syntaxHighlight('init');
    } else {
     this.code = code;
   }
  },

  /**
   * Set the value to be used by the editor
   * @param {String} text The code to be display in editor
   */
  setValue : function(text) {
    this.setCode(text);
  },

  /**
   * Get the value of the code within the editor
   * @return {String} The code within the editor
   */
  getValue : function() {
    return this.getCode();
  },

  /**
   * Toggle autocomplreate on or off
   */
  toggleAutoComplete : function() {
    if (this.editor)
      this.editor.autocomplete = (this.editor.autocomplete) ? false : true;
  },

  /**
   * Toggle readonly on or off
   */
  toggleReadOnly : function() {
    this.textarea.dom.readOnly = (this.textarea.dom.readOnly) ? false : true;
    if(this.iframe.dom.style.display != 'none' && this.editor) // prevent exception on FF + iframe with display:none
      this.editor.readOnly(this.textarea.dom.readOnly ? true : false);
  },

  /**
   * Toggle line numbers on or off
   */
  toggleLineNumbers : function() {
    if (!this.editor) return;
    var cn = this.editor.body.className;
    this.editor.body.className = (cn==''||cn=='show-line-numbers') ? 'hide-line-numbers' : 'show-line-numbers';
  },

  /**
   * Toggle between codepress and textarea
   */
  toggleEditor : function() {
    if(this.textarea.dom.disabled) {
      this.textarea.dom.value = this.getCode();
      this.textarea.dom.disabled = false;
      this.iframe.dom.style.display = 'none';
      this.textarea.dom.style.display = 'inline';
    }
    else {
      this.textarea.dom.disabled = true;
      this.setCode(this.textarea.dom.value);
      if (this.editor) this.editor.syntaxHighlight('init');
      this.iframe.domstyle.display = 'inline';
      this.textarea.dom.style.display = 'none';
    }
  }
});

Ext.ux.CodePress.languages = {
  csharp : 'C#',
  css : 'CSS',
  generic : 'Generic',
  html : 'HTML',
  java : 'Java',
  javascript : 'JavaScript',
  perl : 'Perl',
  ruby : 'Ruby',
  php : 'PHP',
  text : 'Text',
  sql : 'SQL',
  vbscript : 'VBScript',
  pascal : 'Pascal'
}

Ext.reg('codepress', Ext.ux.CodePress);
