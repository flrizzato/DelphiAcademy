/**
 * Creates an HTML file input field on the page. This is usually used to upload files to remote server. File fields are usually
 * created inside a form like this:
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         items: [
 *             {
 *                 xtype: 'fieldset',
 *                 title: 'My Uploader',
 *                 items: [
 *                     {
 *                         xtype: 'filefield',
 *                         label: "MyPhoto:",
 *                         name: 'photo',
 *                         accept: 'image'
 *                     }
 *                 ]
 *             }
 *         ]
 *     });
 */
Ext.define('Ext.field.File', {
    extend: 'Ext.field.Text',
    xtype: 'filefield',

    mixins: [
        'Ext.mixin.ConfigProxy'
    ],

    /**
     * @private
     */
    isFile: true,

    proxyConfigs: {
        fileButton: [
            /**
             * @cfg multiple
             * @inheritdoc Ext.field.FileButton#multiple
             */
            'multiple',

            /**
             * @cfg accept
             * @inheritdoc Ext.field.FileButton#accept
             */
            'accept',

            /**
             * @cfg capture
             * @inheritdoc Ext.field.FileButton#capture
             */
            'capture'
        ]
    },

    readOnly: true,
    editable: false,
    focusable: false,
    inputTabIndex: -1,

    triggers: {
        file: {
            type: 'file'
        }
    },

    classCls: Ext.baseCSSPrefix + 'filefield',

    captureLookup: {
        video: "camcorder",
        image: "camera",
        audio: "microphone"
    },

    onChange: function(me, value, startValue) {
        me.fireEvent('change', this, value, startValue);
    },

    applyName: function (name) {
        var multiple;

        if (name) {
            if (multiple && name.substr(-2, 2) !== "[]") {
                name += "[]";
            } else if ((!multiple) && name.substr(-2, 2) === "[]") {
                name = name.substr(0, name.length - 2)
            }
        }
        return name;
    },

    updateName: function (name) {
        var fileTrigger = this.getTriggers().file,
            inputElement = fileTrigger && fileTrigger.getComponent().buttonElement.dom;

        if (name && inputElement) {
            inputElement.name = name;
        }
    },

    updateMultiple: function () {
        var name = this.getName();
        if (!Ext.isEmpty(name)) {
            this.setName(name);
        }
    },

    updateTriggers: function(triggers, oldTriggers) {
        this.callParent([triggers, oldTriggers]);
        this.getFileButton().on('change', 'onFileButtonChange', this);
    },

    updateValue: function(value, oldValue) {
        this.callParent([value, oldValue]);

        this.getFileButton().setValue(value);
    },

    getFileButton: function() {
        return this.getTriggers().file.getComponent();
    },

    /**
     * Returns the field files.
     * @return {FileList} List of the files selected.
     */
    getFiles: function () {
        return this.getFileButton().getFiles();
    },

    privates: {
        onFileButtonChange: function(fileButton, value) {
            var buttonElement = fileButton.buttonElement.dom,
                files = buttonElement.files,
                len, i, file;

            if (files) {
                value = [];
                for (i = 0, len = files.length; i < len; i++) {
                    file = files[i];
                    value.push(file.name);
                }
                value = value.join(', ');
            }

            this.setValue(value);
        }
    }
});
