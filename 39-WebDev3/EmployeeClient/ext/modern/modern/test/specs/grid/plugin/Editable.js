topSuite('Ext.grid.plugin.Editable', [
    'Ext.grid.Grid'
], function () {
    var grid, plugin, store;

    function createGrid (pluginCfg, cfg) {
        cfg = cfg || {};

        var plugins = cfg.plugins || {};

        plugins.grideditable = pluginCfg || 1;

        grid = Ext.create(Ext.apply({
            xtype: 'grid',
            height: 400,
            renderTo: Ext.getBody(),
            width: 400,
            plugins: plugins,
            columns: [{
                dataIndex: 'name',
                editable: true
            }],
            store: {
                fields: ['name'],
                data: [{
                    name: 'Sencha'
                }]
            }
        }, cfg));

        plugin = grid.getPlugin('grideditable');
        store = grid.getStore();
    }

    function findCell (rowIdx, cellIdx) {
        var row = grid.mapToItem(store.getAt(rowIdx));

        return row.cells[cellIdx].element.dom;
    }

    function waitsForSheetAnim () {
        waitsFor(function () {
            return !plugin.sheet.activeAnimation;
        });
    }

    afterEach(function () {
        grid = store = Ext.destroy(grid);
    });

    describe('sheet', function () {
        it('should open sheet on doubletap', function () {
            createGrid();

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();
        });

        it('should open sheet on tap', function () {
            createGrid({
                triggerEvent: 'childtap'
            });

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'click');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();
        });

        it('should cancel an edit', function () {
            createGrid();

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();

            runs(function () {
                var field = plugin.form.down('textfield'),
                    record = store.getAt(0);

                expect(field.getValue()).toBe('Sencha');

                field.setValue('foobar');

                plugin.onCancelTap();

                expect(record.get('name')).toBe('Sencha');
            });
        });
    });

    describe('form', function () {
        it('should add fields to form', function () {
            createGrid();

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();

            runs(function () {
                var form = plugin.form,
                    fields = form.query('field');

                expect(fields.length).toBe(1);

                expect(fields[0].getValue()).toBe('Sencha');
            });
        });

        it('should warn on duplicate dataIndex', function () {
            createGrid(null, {
                columns: [{
                    dataIndex: 'name',
                    editable: true
                }, {
                    dataIndex: 'name',
                    editable: true
                }]
            });

            var test = function () {
                plugin.getEditorFields(grid.getColumns());
            };

            expect(test).toThrow('An editable column with the same dataIndex "name" already exists.');
        });

        it('should disable submit button', function () {
            createGrid(null, {
                columns: [{
                    dataIndex: 'name',
                    editor: {
                        xtype: 'textfield',
                        required: true
                    }
                }]
            });

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();

            runs(function () {
                var form = plugin.form,
                    field = form.down('field');

                expect(field.getValue()).toBe('Sencha');

                field.setValue('');

                expect(field.isValid()).toBe(false);
                expect(plugin.submitButton.getDisabled()).toBe(true);
            });
        });
    });

    describe('edit record', function () {
        it ('should edit record', function () {
            createGrid();

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();

            runs(function () {
                var field = plugin.form.down('textfield'),
                    record = store.getAt(0);

                expect(field.getValue()).toBe('Sencha');

                field.setValue('foobar');

                plugin.onSubmitTap();

                expect(record.get('name')).toBe('foobar');
            });
        });
    });

    describe('formConfig', function () {
        it('should show custom form', function () {
            createGrid({
                formConfig: {
                    items: [{
                        xtype: 'textfield',
                        name: 'name'
                    }]
                }
            });

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();

            runs(function () {
                var form = plugin.form,
                    fieldset = form.child('fieldset'),
                    fields = form.query('field');

                expect(fieldset).toBeFalsy();

                expect(fields.length).toBe(1);
                expect(fields[0].getValue()).toBe('Sencha');
            });
        });

        it('should allow edit from custom form', function () {
            createGrid({
                formConfig: {
                    items: [{
                        xtype: 'textfield',
                        name: 'name'
                    }]
                }
            });

            var cell = findCell(0, 0);

            jasmine.fireMouseEvent(cell, 'dblclick');

            expect(plugin.sheet.rendered).toBe(true);
            expect(plugin.sheet.getHidden()).toBe(false);

            waitsForSheetAnim();

            runs(function () {
                var form = plugin.form,
                    field = form.child('field'),
                    record = store.getAt(0);

                expect(field.getValue()).toBe('Sencha');

                field.setValue('foobar');

                plugin.onSubmitTap();

                expect(record.get('name')).toBe('foobar');
            });
        });
    });
});
