/**
 * The `formpanel` is an extension over `fieldpanel` and, in addition to rendering the
 * proper `form` tag
 *
 * presents a set of form fields and provides convenient ways to load and
 * save data. This component uses an HTML `form` element to contain its fields, unlike
 * {@link Ext.field.Panel fieldpanel}. As a `form`, this component provides a `submit`
 * method that can be used to post field data to a server.
 *
 * Populating a `formpanel` is the same as with {@link Ext.field.Panel fieldpanel}. The
 * same method are available for setting data in the form, gather field values and managing
 * field errors.
 *
 * ## Submitting Forms
 *
 * Using the {@link Ext.field.Panel fieldpanel} class data can be saved to the server using
 * the {@link Ext.data.Model#method!save save method} of a record. With `formpanel`,
 * however, you can use its submit method.
 *
 *      var panel = Ext.create({
 *          xtype: 'formpanel',
 *          fullscreen: true,
 *
 *          controller: 'mycontroller',
 *
 *          buttons: {
 *              submit: 'onSubmit'
 *          },
 *
 *          items: [{
 *              xtype: 'textfield',
 *              name: 'name',
 *              label: 'Name'
 *          }, {
 *              xtype: 'emailfield',
 *              name: 'email',
 *              label: 'Email'
 *          }, {
 *              xtype: 'passwordfield',
 *              name: 'password',
 *              label: 'Password'
 *          }]
 *      });
 *
 *      Ext.define('MyController', {
 *          extend: 'Ext.app.ViewController',
 *          alias: 'controller.mycontroller',
 *
 *          onSubmit: function () {
 *              var form = this.getView();
 *
 *              form.submit({
 *                  url: 'url/to/submit/to',
 *                  success: function () {
 *                      Ext.Msg.alert('Form submitted successfully!');
 *                  }
 *              });
 *          }
 *      });
 *
 * In this case we provided the `url` to submit the form to inside the submit call.
 * Alternatively you can just set the {@link #url} config when you create the form. We can
 * specify other parameters (see {@link #method!submit} for details), including callback
 * functions for success and failure. These functions are used to take some action in your
 * app after your data has been saved to the server side.
 */
Ext.define('Ext.form.Panel', {
    extend: 'Ext.field.Panel',
    xtype: 'formpanel',

    alternateClassName: 'Ext.form.FormPanel',

    classCls: Ext.baseCSSPrefix + 'formpanel',

    element: {
        reference: 'element',
        tag: 'form',
        novalidate: 'novalidate'
    },

    /**
     * @event submit
     * @preventable
     * Fires upon successful (Ajax-based) form submission.
     * @param {Ext.form.Panel} this This FormPanel.
     * @param {Object} result The result object as returned by the server.
     * @param {Ext.event.Event} e The event object.
     */

    /**
     * @event beforesubmit
     * @preventable
     * Fires immediately preceding any Form submit action.
     * Implementations may adjust submitted form values or options prior to execution.
     * A return value of `false` from this listener will abort the submission
     * attempt (regardless of `standardSubmit` configuration).
     * @param {Ext.form.Panel} this This FormPanel.
     * @param {Object} values A hash collection of the qualified form values to be sent to
     * the server.
     * @param {Object} options Additional options (this is only available when
     * `standardSubmit` is `false`).
     * @param {Ext.event.Event} e The event object if the form was submitted via a HTML5
     * form submit event.
     */

    config: {
        /**
         * @cfg {Boolean} enableSubmissionForm
         * The submission form is generated but never added to the dom. It is a submittable
         * version of your form panel, allowing for fields that are not simple textfields
         * to be properly submitted to servers. It will also send values that are easier
         * to parse with server side code.
         *
         * If this is false we will attempt to subject the raw form inside the form panel.
         */
        enableSubmissionForm: true,

        /**
         * @cfg {String} enctype
         * The enctype attribute for the form, specifies how the form should be encoded
         * when submitting.
         */
        enctype: null,

        /**
         * @cfg {String} method
         * The method which this form will be submitted. `post` or `get`.
         */
        method: 'post',

        /**
         * @cfg {Boolean} multipartDetection
         * If this is enabled the form will automatically detect the need to use
         * 'multipart/form-data' during submission.
         */
        multipartDetection: true,

        /**
         * @cfg {Boolean} standardSubmit
         * Whether or not we want to perform a standard form submit.
         */
        standardSubmit: false,

        /**
         * @cfg {Object} submitOnAction
         * When this is set to `true`, the form will automatically submit itself whenever
         * the `action` event fires on a field in this form. The action event usually fires
         * whenever you press go or enter inside a textfield.
         */
        submitOnAction: false,

        /**
         * @cfg {Boolean} trackResetOnLoad
         * If set to true, {@link #reset}() resets to the last loaded or {@link #setValues}
         * data instead of when the form was first created.
         */
        trackResetOnLoad: false
    },

    getTemplate: function () {
        var template = this.callParent();

        // Added a submit input for standard form submission. This cannot have "display: none;" or it will not work
        template.push({
            tag: 'input',
            type: 'submit',
            cls: Ext.baseCSSPrefix + 'hidden-submit'
        });

        return template;
    },

    /**
     * @private
     */
    initialize: function () {
        this.callParent();
        this.element.on('submit', 'onSubmit', this);
    },

    applyEnctype: function (newValue) {
        var  form = this.element.dom || null;

        if (form) {
            if (newValue) {
                form.setAttribute("enctype", newValue);
            } else {
                form.setAttribute("enctype");
            }
        }
    },

    /**
     * @private
     */
    onSubmit: function (event) {
        var me = this;

        if (event && !me.getStandardSubmit()) {
            event.stopEvent();
        } else {
            this.submit(null, event);
        }
    },

    updateSubmitOnAction: function (value) {
        this[value ? 'on' : 'un']({
            action: 'onFieldAction',
            scope: this
        });
    },

    /**
     * @private
     */
    onFieldAction: function (field) {
        if (this.getSubmitOnAction()) {
            field.blur();
            this.submit();
        }
    },

    /**
     * Performs a Ajax-based submission of form values (if {@link #standardSubmit} is false)
     * or otherwise executes a standard HTML Form submit action.
     *
     * **Notes**
     *
     *  1. Only the first parameter is implemented. Put all other parameters inside the first
     *  parameter:
     *
     *     submit({params: "" ,headers: "" etc.})
     *
     *  2. Submit example:
     *
     *     myForm.submit({
     *       url: 'PostMyData/To',
     *       method: 'Post',
     *       success: function () { Ext.Msg.alert("success"); },
     *       failure: function () { Ext.Msg.alert("error"); }
     *     });
     *
     *  3. Parameters and values only submit for a POST and not for a GET.
     *
     * @param {Object} options
     * The configuration when submitting this form.
     *
     * The following are the configurations when submitting via Ajax only:
     *
     * @param {String} options.url
     * The url for the action (defaults to the form's {@link #url}).
     *
     * @param {String} options.method
     * The form method to use (defaults to the form's {@link #method}, or POST if not defined).
     *
     * @param {Object} options.headers
     * Request headers to set for the action.
     *
     * @param {Boolean} [options.autoAbort=false]
     * `true` to abort any pending Ajax request prior to submission.
     * __Note:__ Has no effect when `{@link #standardSubmit}` is enabled.
     *
     * @param {Number} options.timeout
     * The number is seconds the loading will timeout in.
     *
     * The following are the configurations when loading via Ajax or Direct:
     *
     * @param {String/Object} options.params
     * The params to pass when submitting this form (defaults to this forms {@link #baseParams}).
     * Parameters are encoded as standard HTTP parameters using {@link Ext#urlEncode}.
     *
     * @param {Boolean} [options.submitDisabled=false]
     * `true` to submit all fields regardless of disabled state.
     * __Note:__ Has no effect when `{@link #standardSubmit}` is enabled.
     *
     * @param {String/Object} [options.waitMsg]
     * If specified, the value which is passed to the loading {@link #masked mask}. See
     * {@link #masked} for more information.
     *
     * @param {Function} options.success
     * The callback that will be invoked after a successful response. A response is
     * successful if a response is received from the server and is a JSON object where the
     * `success` property is set to `true`, `{"success": true}`.
     *
     * The function is passed the following parameters and can be used for submitting via
     * Ajax or Direct:
     *
     * @param {Ext.form.Panel} options.success.form
     * The {@link Ext.form.Panel} that requested the action.
     *
     * @param {Object/Ext.direct.Event} options.success.result
     * The result object returned by the server as a result of the submit request. If the
     * submit is sent using Ext Direct, this will return the {@link Ext.direct.Event}
     * instance, otherwise will return an Object.
     *
     * @param {Object} options.success.data
     * The parsed data returned by the server.
     *
     * @param {Function} options.failure
     * The callback that will be invoked after a failed transaction attempt.
     *
     * The function is passed the following parameters and can be used for submitting via
     * Ajax or Direct:
     *
     * @param {Ext.form.Panel} options.failure.form
     * The {@link Ext.form.Panel} that requested the submit.
     *
     * @param {Ext.form.Panel} options.failure.result
     * The failed response or result object returned by the server which performed the
     * operation.
     *
     * @param {Object} options.scope
     * The scope in which to call the callback functions (The `this` reference for the
     * callback functions).
     *
     * @param e
     *
     * @return {Ext.data.Connection} The request object if the {@link #standardSubmit}
     * config is false. If `standardSubmit` is `true`, then the return value is undefined.
     */
    submit: function (options, e) {
        options = options || {};

        var me = this,
            formValues = me.getValues(me.getStandardSubmit() || !options.submitDisabled),
            form = me.element.dom || {};

        if (this.getEnableSubmissionForm()) {
            form = this.createSubmissionForm(form, formValues);
        }

        options = Ext.apply({
            url : me.getUrl() || form.action,
            submit: false,
            form: form,
            method : me.getMethod() || form.method || 'post',
            autoAbort : false,
            params : null,
            waitMsg : null,
            headers : null,
            success : null,
            failure : null
        }, options || {});

        return me.fireAction('beforesubmit', [me, formValues, options, e], 'doBeforeSubmit', null, null, 'after');
    },

    privates: {
        /**
         * @private
         */
        applyExtraParams: function (options) {
            var form = options.form,
                params = Ext.merge(this.getBaseParams() || {}, options.params),
                name, input;

            for (name in params) {
                input = document.createElement('input');
                input.setAttribute('type', 'text');
                input.setAttribute('name', name);
                input.setAttribute('value', params[name]);
                form.appendChild(input);
            }
        },

        /**
         * @private
         */
        beforeAjaxSubmit: function (form, options, successFn, failureFn) {
            var me = this,
                url = options.url || me.getUrl(),
                request = Ext.merge({}, {
                    url: url,
                    timeout: me.getTimeout() * 1000,
                    form: form,
                    scope: me
                }, options);

            delete request.success;
            delete request.failure;

            request.params = Ext.merge(me.getBaseParams() || {}, options.params);
            request.header = Ext.apply({
                'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'
            }, options.headers || {});

            request.callback = function (callbackOptions, success, response) {
                var responseText = response.responseText,
                    responseXML = response.responseXML,
                    statusResult = Ext.data.request.Ajax.parseStatus(response.status, response);

                if (form.$fileswap) {
                    var original, placeholder;

                    Ext.each(form.$fileswap, function (item) {
                        original = item.original;
                        placeholder = item.placeholder;

                        placeholder.parentNode.insertBefore(original, placeholder.nextSibling);
                        placeholder.parentNode.removeChild(placeholder);
                    });

                    form.$fileswap = null;
                    delete form.$fileswap;
                }

                me.setMasked(false);

                if (response.success === false) {
                    success = false;
                }

                if (success) {
                    if (statusResult && responseText && responseText.length === 0) {
                        success = true;
                    } else {
                        if (!Ext.isEmpty(response.responseBytes)) {
                            success = statusResult.success;
                        } else {
                            if (Ext.isString(responseText) && response.request.options.responseType === "text") {
                                response.success = true;
                            } else if (Ext.isString(responseText)) {
                                try {
                                    response = Ext.decode(responseText);
                                } catch (e) {
                                    response.success = false;
                                    response.error = e;
                                    response.message = e.message;
                                }
                            } else if (Ext.isSimpleObject(responseText)) {
                                response = responseText;
                                Ext.applyIf(response, {success: true});
                            }

                            if (!Ext.isEmpty(responseXML)) {
                                response.success = true;
                            }
                            success = !!response.success;
                        }
                    }
                    if (success) {
                        successFn(response, responseText);
                    } else {
                        failureFn(response, responseText);
                    }
                }
                else {
                    failureFn(response, responseText);
                }
            };

            if (Ext.feature.has.XHR2 && request.xhr2) {
                delete request.form;

                var formData = request.data = new FormData(form);

                if (request.params) {
                    Ext.iterate(request.params, function (name, value) {
                        if (Ext.isArray(value)) {
                            Ext.each(value, function (v) {
                                formData.append(name, v);
                            });
                        } else {
                            formData.append(name, value);
                        }
                    });

                    delete request.params;
                }
            }

            return Ext.Ajax.request(request);
        },

        /**
         * @private
         */
        beforeDirectSubmit: function (api, form, options, successFn, failureFn) {
            var me = this,
                submit;

            me.applyExtraParams(options);

            api = Ext.direct.Manager.resolveApi(api, me);
            me.setApi(api);

            submit = api.submit;

            if (!submit) {
                Ext.raise("Cannot find Ext Direct API method for submit action");
            }

            return submit(form, function (data, response, success) {
                me.setMasked(false);

                if (success) {
                    if (data.success) {
                        successFn(response, data);
                    } else {
                        failureFn(response, data);
                    }
                } else {
                    failureFn(response, data);
                }
            }, me);
        },

        /**
         * @private
         */
        beforeStandardSubmit: function (form, options) {
            if (options.url && Ext.isEmpty(form.action)) {
                form.action = options.url;
            }

            // Spinner fields must have their components enabled *before* submitting or else the value
            // will not be posted.
            var fields = this.query('spinnerfield'),
                ln = fields.length,
                body = document.body,
                i, field;

            for (i = 0; i < ln; i++) {
                field = fields[i];

                if (!field.getDisabled()) {
                    field.setDisabled(false);
                }
            }

            body.appendChild(form);

            form.method = (options.method || form.method).toLowerCase();
            form.submit();

            // If the form is targeting a different DOM such as an iframe, then this
            // resource will remain in the current body since it does not reload upon
            // submit so we need to explicitly remove it.
            body.removeChild(form);
        },

        /**
         * @private
         */
        createSubmissionForm: function (form, values) {
            var fields = this.getFields(),
                name, input, field, fileTrigger, inputDom;

            if (form.nodeType === 1) {
                form = form.cloneNode(false);

                for (name in values) {
                    input = document.createElement("input");
                    input.setAttribute("type", "text");
                    input.setAttribute("name", name);
                    input.setAttribute("value", values[name]);
                    form.appendChild(input);
                }
            }

            for (name in fields) {
                if (fields.hasOwnProperty(name)) {
                    field = fields[name];
                    if(field.isFile) {
                        // The <input type="file"> of a FileField is its "file" trigger button.
                        fileTrigger = field.getTriggers().file;
                        inputDom = fileTrigger && fileTrigger.getComponent().buttonElement.dom;

                        if (inputDom) {
                            if(!form.$fileswap) form.$fileswap = [];
                            input = inputDom.cloneNode(true);
                            inputDom.parentNode.insertBefore(input, inputDom.nextSibling);
                            form.appendChild(inputDom);
                            form.$fileswap.push({original: inputDom, placeholder: input});
                        }
                    } else if(field.isPassword) {
                        if(field.getInputType() !== "password") {
                            field.setRevealed(false);
                        }
                    }
                }
            }

            return form;
        },

        /**
         * @private
         */
        doBeforeSubmit: function (me, formValues, options) {
            var form = options.form || {},
                multipartDetected = false,
                ret;

            if (this.getMultipartDetection() === true) {
                this.getFields(false).forEach(function (field) {
                    if (field.isFile === true) {
                        multipartDetected = true;
                        return false;
                    }
                });

                if (multipartDetected) {
                    form.setAttribute("enctype", "multipart/form-data");
                }
            }

            if (options.enctype) {
                form.setAttribute("enctype", options.enctype);
            }

            if (me.getStandardSubmit()) {
                ret = me.beforeStandardSubmit(form, options);
            }
            else {
                var api = me.getApi(),
                    scope = options.scope || me,
                    failureFn = function (response, responseText) {
                        if (Ext.isFunction(options.failure)) {
                            options.failure.call(scope, me, response, responseText);
                        }

                        me.fireEvent('exception', me, response);
                    },
                    successFn = function (response, responseText) {
                        if (Ext.isFunction(options.success)) {
                            options.success.call(options.scope || me, me, response, responseText);
                        }

                        me.fireEvent('submit', me, response);
                    },
                    waitMsg = options.waitMsg;

                if (options.waitMsg) {
                    if (typeof waitMsg === 'string') {
                        waitMsg = {
                            xtype: 'loadmask',
                            message: waitMsg
                        };
                    }

                    me.setMasked(waitMsg);
                }

                if (api) {
                    ret = me.beforeDirectSubmit(api, form, options, successFn, failureFn);
                }
                else {
                    ret = me.beforeAjaxSubmit(form, options, successFn, failureFn);
                }
            }

            return ret;
        }
    }
});
