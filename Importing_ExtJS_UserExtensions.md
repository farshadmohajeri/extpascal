## Introduction ##

The Ext JS community has produced various free extensions for Ext JS framework, increasing the Ext JS funcionality and power. This tutorial shows how importing these extensions into ExtPascal.

## Where to find Ext JS User Extensions(UX)? ##
Some links to good extensions:
  * http://extjs.eu
  * http://extjs-ux.org
  * http://www.siteartwork.de/livegrid/
  * http://www.siteartwork.de/gridviewmenuplugin
  * http://www.siteartwork.de/youtubeplayer/
  * http://www.siteartwork.de/wizardcomponent
  * http://e2cs.mm-mendez.com/?page_id=3&language=en
  * http://technomedia.co.uk/SuperBoxSelect/examples3.html

> or googling: ext.ux

## Steps ##
  1. Copy the extension sources to folder **ux** below **htdocs** folder
  1. (Optional) Rename the original sources (.js and .css files) appending "-debug" string to the file name. By example:
```
Ext.ux.form.LovCombo.js to Ext.ux.form.LovCombo-debug.js
Ext.ux.form.LovCombo.css to Ext.ux.form.LovCombo-debug.css
```
  1. (Optional) Optimize the sources (.js and .css files) using this online [JavaScript Compressor](http://www.javascriptcompressor.com). The optimized sources will retain the original names without "-debug"
  1. Read the extension docs and the original source to identify properties and methods
  1. Update the ExtFixes.txt file, in ExtJSWrapper folder, declaring:
    1. The component itself, inform:
      1. Object Pascal new name, by example: ExtUxFormLovCombo
      1. Parent class name or empty, by example: ExtFormComboBox
      1. Unit name: ExtForm
      1. JavaScript class name: Ext.ux.form.LovCombo
      1. Put then this line in ExtFixes.txt:
```
ExtUxFormLovCombo, ExtFormComboBox, ExtForm, Ext.ux.form.LovCombo
```
    1. Each property
      1. Class name: ExtUxFormLovCombo
      1. JavaScript property name: separator
      1. Object Pascal type: string
      1. If is a static property: false
      1. If is a config option: true
      1. Default value or FORCEADD for alternate type properties: empty in this case
      1. Put then this line in ExtFixes.txt:
```
ExtUxFormLovCombo, separator, string, false, true,
```
      1. If the property is a enumeration add this line too:
```
ExtUxFormLovCombo, separator, (spCenter, spNormal, spLeft, spRight)
```
    1. Each method
      1. Class name, by example: ExtUxGridRecordForm
      1. JavaScript method name, use Create if is constructor: show
      1. Return type or void or empty if a constructor: void
      1. If is a static method: false
      1. if is a overload method: false
        1. For each method parameter
          1. Parameter name: DataRecord
          1. Parameter Object Pascal type: ExtDataRecord
          1. if is a optional parameter: false
      1. Put then this line in ExtFixes.txt:
```
ExtUxGridRecordForm, show, void, false, false, DataRecord, ExtDataRecord, false, animEl, ExtElement, true
```
    1. Each event
      1. Class name: ExtButton
      1. JavaScript event name: Menuhide
      1. The literal 'Event': Event
        1. For each event parameter
          1. Parameter name: This
          1. Type: ExtButton
      1. Put then this line in ExtFixes.txt:
```
ExtButton, Menuhide, Event, This, ExtButton, Menu, ExtObject
```
  1. In folder ExtJSWrapper compile ExtToPascal.dpr
  1. Regenerate the ExtJS wrappers using ExToPascal
```
ExtToPascal <ExtJS docs path>
```
  1. In your ExtPascal application, in the method that will use this extension, add this line, by example:
```
SetLibrary('/ux/Ext.ux.form.LovCombo', true{if has a CSS companion file});
```