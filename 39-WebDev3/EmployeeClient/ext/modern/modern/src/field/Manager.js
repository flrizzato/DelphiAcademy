/**
 * This mixin is used by {@link Ext.field.Panel fieldpanel} to provide field management
 * methods.
 *
 * ## Setting Form Data
 *
 * The {@link #setValues} can be used to populate fields from a data object.
 *
 *     form.setValues({
 *         name: 'Peter',
 *         email: 'peter.venkman@gb.com',
 *         password: 'secret'
 *     });
 *
 * It's also easy to load {@link Ext.data.Model Model} instances into a form - let's say
 * we have a User model and want to load a particular instance into our form:
 *
 *     Ext.define('MyApp.model.User', {
 *         extend: 'Ext.data.Model',
 *         config: {
 *             fields: ['name', 'email', 'password']
 *         }
 *     });
 *
 *     var ed = Ext.create('MyApp.model.User', {
 *         name: 'Peter',
 *         email: 'peter.venkman@gb.com',
 *         password: 'secret'
 *     });
 *
 *     form.setRecord(ed);
 *
 * ## Setting multiple errors on fields
 *
 * While you can call {@link Ext.field.Field#setError} and
 * {@link Ext.field.Field#setError setError(null)} on each field, in your form, FormPanel provides a
 * {@link #setErrors} method that will apply an Object of error states to multiple fields
 * with one call:
 *
 *      panel.setErrors({
 *          field1: 'field1 is in error',
 *          name2: 'field2 is in error',
 *          fieldname3: null,   // clear any errors
 *          fieldname4: [ 'one', 'two', 'three' },      // multiple errors
 *          ...
 *      });
 *
 * While you can call {@link Ext.field.Field#getError} on each field in the form to query
 * the form for errors, you can call {@link #getError} on the form to get an array of
 * error states, suitable to pass into {@link #setErrors}.
 *
 * NOTE: these methods will only work on fields with a {@link Ext.field.Field#cfg!name name}
 * config specified.
 * @protected
 * @since 6.5.0
 */
Ext.define('Ext.field.Manager', {
    mixinId: 'fieldmanager',

    requires: [
        'Ext.Ajax'
    ],

    /**
     * Set the fields of the provided `record` from the {@link Ext.field.Field#cfg!name named}
     * fields.
     * @param {Ext.data.Model} record
     * @return {Ext.field.Manager} this
     */
    fillRecord: function (record) {
        var values, name;

        if (record) {
            values = this.getValues();

            for (name in values) {
                if (values.hasOwnProperty(name) && record.getField(name)) {
                    record.set(name, values[name]);
                }
            }
        }

        return this;
    },

    consumeRecord: function(record) {
        var data = record && record.data;
        if (data) {
            this.setValues(data);
        }
    },

    /**
     * Sets the values of form fields in bulk. Example usage:
     *
     *     myForm.setValues({
     *         name: 'Bill',
     *         crazy: true,
     *         username: 'bill.preston'
     *     });
     *
     * If there groups of checkbox fields with the same name, pass their values in an
     * array. For example:
     *
     *     myForm.setValues({
     *         name: 'Ted',
     *         crazy: false,
     *         hobbies: [
     *             'reading',
     *             'cooking',
     *             'gaming'
     *         ]
     *     });
     *
     * @param {Object} values field name => value mapping object.
     * @return {Ext.field.Manager} this
     */
    setValues: function (values) {
        var fields = this.getFields(),
            name, field, value, ln, i, f;

        values = values || {};

        for (name in values) {
            if (values.hasOwnProperty(name)) {
                field = fields[name];
                value = values[name];

                if (field) {
                    // If there are multiple fields with the same name. Checkboxes, radio
                    // fields and maybe event just normal fields..
                    if (Ext.isArray(field)) {
                        ln = field.length;

                        // Loop through each of the fields
                        for (i = 0; i < ln; i++) {
                            f = field[i];

                            if (f.isRadio) {
                                // If it is a radio field just use setGroupValue which
                                // will handle all of the radio fields
                                f.setGroupValue(value);
                                break;
                            } else if (f.isCheckbox) {
                                if (Ext.isArray(value)) {
                                   f.setChecked((value.indexOf(f._value) != -1));
                               } else {
                                   f.setChecked((value == f._value));
                               }
                            } else {
                                // If it is a bunch of fields with the same name, check
                                // if the value is also an array, so we can map it to
                                // each field
                                if (Ext.isArray(value)) {
                                    f.setValue(value[i]);
                                }
                            }
                        }
                    } else {
                        if (field.isRadio || field.isCheckbox) {
                            // If the field is a radio or a checkbox
                            field.setChecked(value);
                        } else {
                            // If just a normal field
                            field.setValue(value);
                        }
                    }

                    if (this.getTrackResetOnLoad && this.getTrackResetOnLoad()) {
                       field.resetOriginalValue();
                    }
                }
            }
        }

        return this;
    },

    /**
     * Returns an object containing the value of each field in the form, keyed to the
     * field's name.
     *
     * For groups of checkbox fields with the same name, it will be arrays of values.
     * For example:
     *
     *     {
     *         name: "Bill", // From a TextField
     *         favorites: [
     *             'pizza',
     *             'noodle',
     *             'cake'
     *         ]
     *     }
     *
     * @param {Boolean} [enabled] `true` to return only enabled fields.
     * @param {Boolean} [all] `true` to return all fields even if they don't have a
     * {@link Ext.field.Field#name name} configured.
     * @return {Object} Object mapping field name to its value.
     */
    getValues: function (enabled, all) {
        var fields = this.getFields(),
            values = {},
            isArray = Ext.isArray,
            field, value, addValue, bucket, name, ln, i;

        // Function which you give a field and a name, and it will add it into the values
        // object accordingly
        addValue = function(field, name) {
            if (!all && (!name || name === 'null') || field.isFile) {
                return;
            }

            if (field.isCheckbox) {
                value = field.getSubmitValue();
            } else {
                value = field.getValue();
            }

            if (!(enabled && field.getDisabled())) {
                // RadioField is a special case where the value returned is the fields valUE
                // ONLY if it is checked
                if (field.isRadio) {
                    if (field.isChecked()) {
                        values[name] = value;
                    }
                } else {
                    // Check if the value already exists
                    bucket = values[name];

                    if (!Ext.isEmpty(bucket)) {
                        if (!field.isCheckbox || field.isChecked()) {
                            // if it does and it isn't an array, we need to make it into an array
                            // so we can push more
                            if (!isArray(bucket)) {
                                bucket = values[name] = [bucket];
                            }

                            // Check if it is an array
                            if (isArray(value)) {
                                // Concat it into the other values
                                bucket = values[name] = bucket.concat(value);
                            } else {
                                // If it isn't an array, just pushed more values
                                bucket.push(value);
                            }
                        }
                    } else {
                        values[name] = value;
                    }
                }
            }
        };

        // Loop through each of the fields, and add the values for those fields.
        for (name in fields) {
            if (fields.hasOwnProperty(name)) {
                field = fields[name];

                if (isArray(field)) {
                    ln = field.length;
                    for (i = 0; i < ln; i++) {
                        addValue(field[i], name);
                    }
                } else {
                    addValue(field, name);
                }
            }
        }

        return values;
    },

    /**
     * Resets all fields in the form back to their original values.
     * @param {boolean} clearInvalid If `true` will clear any invalid UI state for the fields
     * as well.
     * @return {Ext.field.Manager} this
     */
    reset: function (clearInvalid) {
        this.getFields(false).forEach(function (field) {
            field.reset();

            if (clearInvalid) {
                field.setError(null);
            }
        });

        return this;
    },

    /**
     * A convenient method to disable all fields in this form.
     * @return {Ext.field.Manager} this
     */
    updateDisabled: function (newDisabled) {
        this.getFields(false).forEach(function (field) {
            field.setDisabled(newDisabled);
        });

        return this;
    },

    /**
     * Marks multiple fields valid or invalid based upon an Object of error states
     *
     * Each key of the error states Object is the name of the field whose validity status
     * is to be affected. Each value of the error states Object is either a string or array
     * of strings to set as the field's invalid message(s). If the value is null or an
     * empty array, the field is marked valid instead of invalid.
     *
     * @param {Object} errors The errors to set child fields with.
     * @return {Ext.field.Manager} this
     */
    setErrors: function (errors) {
        var setError = function (field, fieldname) {
                if (field) {
                    messages = errors[fieldname];

                    if (messages === null || (Ext.isArray(messages) && messages.length === 0)) {
                        field.setError(null);
                    }
                    else {
                        field.setError(messages);
                    }
                }
            },
            fieldname, field, messages,
            i, length;

        //<debug>
        if (!Ext.isObject(errors)) {
            Ext.raise('setErrors requires an Object parameter');
        }
        //</debug>

        for (fieldname in errors) {
            field = this.lookupName(fieldname) || this.lookup(fieldname);

            if (Ext.isArray(field)) {
                for (i = 0, length = field.length; i < length; i++) {
                    setError(field[i], fieldname);
                }
            } else {
                setError(field, fieldname);
            }
        }

        return this;
    },

    /**
     * Marks all named fields as valid by calling setError() on each.
     *
     * @return {Ext.field.Manager} this
     */
    clearErrors: function () {
        var fields = this.getFields(false),
            i, length, field;

        for (i = 0, length = fields.length; i < length; i++) {
            field = fields[i];

            if (field.getName() && field.setError) {
                field.setError(null);
            }
        }

        return this;
    },

    /**
     * Gets error state for all named fields of the form.
     *
     * The Object returned is exactly the same as one that can be passed to {@link #setErrors}.
     */
    getErrors: function () {
        var errors = {},
            fields = this.getFields(false).filter(function (field) {
                return field.getName();
            }),
            i, length, field, error;

        for (i = 0, length = fields.length; i < length; i++) {
            field = fields[i];
            error = field.getError();

            if (!error || !error.length) {
                error = null;
            }

            errors[field.getName()] = error;
        }

        return errors;
    },

    /**
     * Test to see if the form has any invalid fields.
     *
     * **NOTE** This method does not validate the fields, it only returns
     * `true` if any field is already marked invalid using the field's
     * {@link Ext.field.Field#isValid isValid}. If field values need to
     * be validated, {@link #validate} should be used instead.
     *
     * @return {Boolean} `true` if all fields are currently valid.
     */
    isValid: function () {
        var fields = this.getFields(false),
            i, length;

        for (i = 0, length = fields.length; i < length; i++) {
            if (!fields[i].isValid()) {
                return false;
            }
        }

        return true;
    },

    /**
     * Calls {@link Ext.field.Field#validate validate} on each field in the form.
     *
     * **Note** This will validate the field's current value. If you want to check if
     * all the fields are currently valid without validating the fields value,
     * {@link #isValid} should be used instead.
     *
     * @param {Boolean} [skipLazy] (private) Pass `true` to skip validators marked as `lazy`.
     * @return {Boolean} `true` if all fields in the form are valid, false if
     * any one (or more) of the fields is invalid.
     */
    validate: function (skipLazy) {
        var fields = this.getFields(false),
            valid = true,
            i, length;

        for (i = 0, length = fields.length; i < length; i++) {
            if (!fields[i].validate(skipLazy)) {
                //don't stop the loop, need to validate all fields
                //so all fields can show validation status
                valid = false;
            }
        }
        return valid;
    },

    /**
     * Returns all {@link Ext.field.Field field} instances inside this form.
     * @param {String} byName Return an array of fields that match the given name.
     * If only one field matches the name, only that instance will be returned.
     * @param {Boolean} deep `false` will prevent it from getting fields
     * in child containers.
     * @return {Object/Ext.field.Field/Ext.field.Field[]} All field instances, mapped by field name;
     * or an array if `byName` is passed.
     */
    getFields: function (byName, deep) {
        var selector = (deep === false ? '> ' : '') + 'field' + (byName ? '[name=' + byName + ']' : ''),
            fields = this.query(selector),
            asArray = byName === false,
            obj, i, length, field, name, bucket;

        if (!fields && asArray) {
            // no fields were found but we want an array
            return [];
        } else if (fields && !asArray) {
            // we weren't told to alway return an array
            if (!byName) {
                // no name was provided so we need to build a map
                // of field names to instance(s)
                obj = {};

                for (i = 0, length = fields.length; i < length; i++) {
                    field = fields[i];
                    name = field.getName();
                    bucket = obj[name];

                    if (bucket) {
                        if (Ext.isArray(bucket)) {
                            bucket.push(field);
                        } else {
                            obj[name] = [bucket, field];
                        }
                    } else {
                        obj[name] = field;
                    }
                }

                return obj;
            } else if (fields.length < 2) {
                // since asArray is falsy and there is 1 or less fields
                // if length is 0, will return undefined
                return fields[0];
            }
        }

        return fields;
    },

    /**
     * Returns the currently focused field
     * @return {Ext.field.Field} The currently focused field, if one is focused or `null`.
     * @private
     */
    getFocusedField: function () {
        var fields = this.getFields(false),
            ln = fields.length,
            field, i;

        for (i = 0; i < ln; i++) {
            field = fields[i];

            if (field.hasFocus) {
                return field;
            }
        }

        return null;
    },

    /**
     * @return {Boolean/Ext.field.Field} The next field if one exists, or `false`.
     * @private
     */
    getNextField: function () {
        var fields = this.getFields(false),
            focusedField = this.getFocusedField(),
            index;

        if (focusedField) {
            index = fields.indexOf(focusedField);

            if (index !== fields.length - 1) {
                index++;
                return fields[index];
            }
        }

        return false;
    },

    /**
     * Tries to focus the next field in the form, if there is currently a focused field.
     * @return {Boolean/Ext.field.Field} The next field that was focused, or `false`.
     * @private
     */
    focusNextField: function () {
        var field = this.getNextField();

        if (field) {
            field.focus();
            return field;
        }

        return false;
    },

    /**
     * @private
     * @return {Boolean/Ext.field.Field} The next field if one exists, or `false`.
     */
    getPreviousField: function () {
        var fields = this.getFields(false),
            focusedField = this.getFocusedField(),
            index;

        if (focusedField) {
            index = fields.indexOf(focusedField);

            if (index !== 0) {
                index--;
                return fields[index];
            }
        }

        return false;
    },

    /**
     * Tries to focus the previous field in the form, if there is currently a focused field.
     * @return {Boolean/Ext.field.Field} The previous field that was focused, or `false`.
     * @private
     */
    focusPreviousField: function () {
        var field = this.getPreviousField();

        if (field) {
            field.focus();
            return field;
        }

        return false;
    }
});
