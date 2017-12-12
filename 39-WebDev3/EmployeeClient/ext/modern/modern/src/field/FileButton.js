/**
 * @private
 */
Ext.define('Ext.field.FileButton', {
    extend: 'Ext.Button',
    xtype: 'filebutton',

    /**
     * @event change
     * Fires when the value has changed.
     * @param {Ext.field.FileButton} this This FileButton
     * @param {String} newValue The new value
     * @param {String} oldValue The original value
     */

    config: {
        /**
         * @cfg {String}
         * The value of the file button's input
         */
        value: null,

        /**
         * @cfg {Boolean}
         * Allow selection of multiple files
         */
        multiple: false,

        /**
         * @cfg {String}
         * File input accept attribute documented here (http://www.w3schools.com/tags/att_input_accept.asp)
         * Also can be simple strings -- e.g. audio, video, image
         */
        accept: null,

        /**
         * @cfg {String}
         * File input capture attribute. Accepts values such as "camera", "camcorder", "microphone"
         */
        capture: null
    },

    buttonType: 'file',
    ui: 'action',
    text: 'Browse...',
    preventDefaultAction: false,
    keyHandlersAdded: true,

    eventHandlers: {
        change: 'onChange'
    },

    getButtonTemplate: function() {
        var template = this.callParent();

        template.tag = 'input';
        template.onchange = 'return Ext.doEv(this, event);';

        return template;
    },

    applyAccept: function (value) {
        switch (value) {
            case "video":
            case "audio":
            case "image":
                value = value + "/*";
                break;
        }

        this.setInputAttribute('accept', value);
    },

    applyCapture: function (value) {
        this.setInputAttribute('capture', value);
        return value;
    },

    applyMultiple: function (value) {
        this.setInputAttribute('multiple', value ? '' : null);
        return value;
    },

    onChange: function(e) {
        this.setValue(this.buttonElement.dom.value);
    },

    updateValue: function(value, oldValue) {
        this.fireEvent('change', this, value, oldValue);
    },

    /**
     * Returns the field files.
     * @return {FileList} List of the files selected.
     */
    getFiles: function () {
        return this.buttonElement.dom.files;
    },

    privates: {
        setInputAttribute: function (attribute, newValue) {
            var buttonElement = this.buttonElement.dom;

            if (!Ext.isEmpty(newValue, true)) {
                buttonElement.setAttribute(attribute, newValue);
            }
            else {
                buttonElement.removeAttribute(attribute);
            }
        }
    }
});
