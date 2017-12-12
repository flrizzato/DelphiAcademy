describe('Ext.field.Field_layout', function() {
    var Field = Ext.define(null, {
            extend: Ext.field.Field,
            getBodyTemplate: function () {
                return [{
                    reference: 'contentElement',
                    style: 'height: 24px; width: 200px; background: red;'
                }];
            }
        }),
        labelHtml = '<span style="display:inline-block;width:50px;background:green;">&nbsp;</span>',
        container, field;

    function create(config, containerConfig) {
        field = new Field(Ext.apply({
            label: labelHtml
        }, config));

        field.labelHtmlElement = field.labelTextElement.first();

        if (containerConfig === null && !field.getFloated()) {
            field.render(Ext.getBody());
        } else {
            container = new Ext.Container(Ext.apply({
                renderTo: Ext.getBody(),
                width: 600,
                items: field
            }, containerConfig));
        }

        if (field.getFloated()) {
            field.show();
        }
    }

    function setError(msg) {
        field.setError(msg || 'Err');
    }

    afterEach(function() {
        if (container) {
            container.destroy();
            container = null;
        }

        if (field) {
            field.destroy();
            field = null;
        }
    });

    describe('auto size', function() {
        it('should layout with left label and side error', function() {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '100 0 474 24' },
                labelElement: { xywh: '0 0 100 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '574 4 26 16' },
                errorIconElement: { xywh: '579 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function() {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '26 0 474 24' },
                labelElement: { xywh: '500 0 100 24' },
                contentElement: { xywh: '26 0 200 24' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and side error', function() {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 48' },
                bodyElement: { xywh: '0 24 574 24' },
                labelElement: { xywh: '0 0 600 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '574 28 26 16' },
                errorIconElement: { xywh: '579 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function() {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 48' },
                bodyElement: { xywh: '0 0 574 24' },
                labelElement: { xywh: '0 24 600 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '574 4 26 16' },
                errorIconElement: { xywh: '579 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 44' },
                bodyElement: { xywh: '100 0 500 24' },
                labelElement: { xywh: '0 0 100 44' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '100 26 500 16' },
                errorIconElement: { xywh: '100 26 16 16' },
                errorMessageElement: { xywh: '121 26 479 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 44' },
                bodyElement: { xywh: '0 0 500 24' },
                labelElement: { xywh: '500 0 100 44' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 500 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 479 16' },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 68' },
                bodyElement: { xywh: '0 24 600 24' },
                labelElement: { xywh: '0 0 600 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '0 50 600 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 579 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 68' },
                bodyElement: { xywh: '0 0 600 24' },
                labelElement: { xywh: '0 44 600 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 600 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 579 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('configured width - larger than content width', function() {
        it('should layout with left label and side error', function () {
            create({
                width: 400,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 24' },
                labelElement: { xywh: '0 0 100 24' },
                bodyElement: { xywh: '100 0 274 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '374 4 26 16' },
                errorIconElement: { xywh: '379 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                width: 400,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 24' },
                labelElement: { xywh: '300 0 100 24' },
                bodyElement: { xywh: '26 0 274 24' },
                contentElement: { xywh: '26 0 200 24' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '306 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                width: 400,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 48' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 374 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '374 28 26 16' },
                errorIconElement: { xywh: '379 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                width: 400,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 48' },
                labelElement: { xywh: '0 24 400 24' },
                bodyElement: { xywh: '0 0 374 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '374 4 26 16' },
                errorIconElement: { xywh: '379 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                width: 400,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 44' },
                labelElement: { xywh: '0 0 100 44' },
                bodyElement: { xywh: '100 0 300 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '100 26 300 16' },
                errorIconElement: { xywh: '100 26 16 16' },
                errorMessageElement: { xywh: '121 26 279 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                width: 400,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 44' },
                labelElement: { xywh: '300 0 100 44' },
                bodyElement: { xywh: '0 0 300 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 300 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 279 16' },
                labelHtmlElement: { xywh: '306 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                width: 400,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 68' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 400 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '0 50 400 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 379 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                width: 400,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 68' },
                labelElement: { xywh: '0 44 400 24' },
                bodyElement: { xywh: '0 0 400 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 400 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 379 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('configured width - smaller than content width', function () {
        it('should layout with left label and side error', function () {
            create({
                width: 150,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 24' },
                labelElement: { xywh: '0 0 100 24' },
                bodyElement: { xywh: '100 0 24 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '124 4 26 16' },
                errorIconElement: { xywh: '129 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                width: 150,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 24' },
                labelElement: { xywh: '50 0 100 24' },
                bodyElement: { xywh: '26 0 24 24' },
                contentElement: { xywh: '26 0 200 24' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '56 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                width: 150,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 48' },
                labelElement: { xywh: '0 0 150 24' },
                bodyElement: { xywh: '0 24 124 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '124 28 26 16' },
                errorIconElement: { xywh: '129 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                width: 150,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 48' },
                labelElement: { xywh: '0 24 150 24' },
                bodyElement: { xywh: '0 0 124 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '124 4 26 16' },
                errorIconElement: { xywh: '129 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                width: 150,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 44' },
                labelElement: { xywh: '0 0 100 44' },
                bodyElement: { xywh: '100 0 50 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '100 26 50 16' },
                errorIconElement: { xywh: '100 26 16 16' },
                errorMessageElement: { xywh: '121 26 29 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                width: 150,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 44' },
                labelElement: { xywh: '50 0 100 44' },
                bodyElement: { xywh: '0 0 50 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 50 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 29 16' },
                labelHtmlElement: { xywh: '56 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                width: 150,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 68' },
                labelElement: { xywh: '0 0 150 24' },
                bodyElement: { xywh: '0 24 150 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '0 50 150 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 129 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                width: 150,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 150 68' },
                labelElement: { xywh: '0 44 150 24' },
                bodyElement: { xywh: '0 0 150 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 150 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 129 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('configured width - smaller than label width', function () {
        it('should layout with left label and side error', function () {
            create({
                width: 50,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 24' },
                labelElement: { xywh: '0 0 50 24' },
                bodyWrapElement: { xywh: '50 0 0 24' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                width: 50,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 24' },
                labelElement: { xywh: '0 0 50 24' },
                bodyWrapElement: { xywh: '0 0 0 24' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                width: 50,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 48' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 24 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '24 28 26 16' },
                errorIconElement: { xywh: '29 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                width: 50,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 48' },
                labelElement: { xywh: '0 24 50 24' },
                bodyElement: { xywh: '0 0 24 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '24 4 26 16' },
                errorIconElement: { xywh: '29 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                width: 50,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError('E');

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 44' },
                labelElement: { xywh: '0 0 50 44' },
                bodyWrapElement: { xywh: '50 0 0 44' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                width: 50,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError('E');

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 44' },
                labelElement: { xywh: '0 0 50 44' },
                bodyWrapElement: { xywh: '0 0 0 44' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                width: 50,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 68' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 50 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '0 50 50 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 29 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                width: 50,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 68' },
                labelElement: { xywh: '0 44 50 24' },
                bodyElement: { xywh: '0 0 50 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 50 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 29 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('flex grow width', function() {
        var containerConfig = {
            width: 400,
            layout: 'hbox',
            defaults: {
                flex: 1
            }
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 274 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '374 4 26 16'},
                errorIconElement: {xywh: '379 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 24'},
                labelElement: {xywh: '300 0 100 24'},
                bodyElement: {xywh: '26 0 274 24'},
                contentElement: {xywh: '26 0 200 24'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 48'},
                labelElement: {xywh: '0 0 400 24'},
                bodyElement: {xywh: '0 24 374 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '374 28 26 16'},
                errorIconElement: {xywh: '379 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 48'},
                labelElement: {xywh: '0 24 400 24'},
                bodyElement: {xywh: '0 0 374 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '374 4 26 16'},
                errorIconElement: {xywh: '379 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 300 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '100 26 300 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 279 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 44'},
                labelElement: {xywh: '300 0 100 44'},
                bodyElement: {xywh: '0 0 300 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 300 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 279 16'},
                labelHtmlElement: {xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 68'},
                labelElement: {xywh: '0 0 400 24'},
                bodyElement: {xywh: '0 24 400 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 400 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 379 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 68'},
                labelElement: {xywh: '0 44 400 24'},
                bodyElement: {xywh: '0 0 400 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 400 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 379 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('flex shrink width - smaller than content width', function () {
        var containerConfig = {
            width: 150,
            layout: 'hbox',
            defaults: {
                flex: 1
            }
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 24 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '124 4 26 16'},
                errorIconElement: {xywh: '129 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 24'},
                labelElement: {xywh: '50 0 100 24'},
                bodyElement: {xywh: '26 0 24 24'},
                contentElement: {xywh: '26 0 200 24'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '56 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 48'},
                labelElement: {xywh: '0 0 150 24'},
                bodyElement: {xywh: '0 24 124 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '124 28 26 16'},
                errorIconElement: {xywh: '129 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 48'},
                labelElement: {xywh: '0 24 150 24'},
                bodyElement: {xywh: '0 0 124 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '124 4 26 16'},
                errorIconElement: {xywh: '129 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 50 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '100 26 50 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 29 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 44'},
                labelElement: {xywh: '50 0 100 44'},
                bodyElement: {xywh: '0 0 50 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 50 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 29 16'},
                labelHtmlElement: {xywh: '56 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 68'},
                labelElement: {xywh: '0 0 150 24'},
                bodyElement: {xywh: '0 24 150 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 150 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 129 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 68'},
                labelElement: {xywh: '0 44 150 24'},
                bodyElement: {xywh: '0 0 150 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 150 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 129 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('flex shrink width - smaller than label width', function () {
        var containerConfig = {
            width: 50,
            layout: 'hbox',
            defaults: {
                flex: 1
            }
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 24'},
                labelElement: {xywh: '0 0 50 24'},
                bodyWrapElement: {xywh: '50 0 0 24'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 24'},
                labelElement: {xywh: '0 0 50 24'},
                bodyWrapElement: {xywh: '0 0 0 24'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 24 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '24 28 26 16'},
                errorIconElement: {xywh: '29 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 24 50 24'},
                bodyElement: {xywh: '0 0 24 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '24 4 26 16'},
                errorIconElement: {xywh: '29 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 44'},
                labelElement: {xywh: '0 0 50 44'},
                bodyWrapElement: {xywh: '50 0 0 44'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 44'},
                labelElement: {xywh: '0 0 50 44'},
                bodyWrapElement: {xywh: '0 0 0 44'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 50 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 50 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 29 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 44 50 24'},
                bodyElement: {xywh: '0 0 50 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 50 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 29 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('configured height', function () {
        it('should layout with left label and side error', function () {
            create({
                height: 100,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '100 0 474 100' },
                labelElement: { xywh: '0 0 100 100' },
                contentElement: { xywh: '100 38 200 24' },
                errorElement: { xywh: '574 42 26 16' },
                errorIconElement: { xywh: '579 42 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                height: 100,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '26 0 474 100' },
                labelElement: { xywh: '500 0 100 100' },
                contentElement: { xywh: '26 38 200 24' },
                errorElement: { xywh: '0 42 26 16' },
                errorIconElement: { xywh: '5 42 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                height: 100,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '0 24 574 76' },
                labelElement: { xywh: '0 0 600 24' },
                contentElement: { xywh: '0 50 200 24' },
                errorElement: { xywh: '574 54 26 16' },
                errorIconElement: { xywh: '579 54 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                height: 100,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '0 0 574 76' },
                labelElement: { xywh: '0 76 600 24' },
                contentElement: { xywh: '0 26 200 24' },
                errorElement: { xywh: '574 30 26 16' },
                errorIconElement: { xywh: '579 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 80 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                height: 100,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '100 0 500 80' },
                labelElement: { xywh: '0 0 100 100' },
                contentElement: { xywh: '100 28 200 24' },
                errorElement: { xywh: '100 82 500 16' },
                errorIconElement: { xywh: '100 82 16 16' },
                errorMessageElement: { xywh: '121 82 479 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                height: 100,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '0 0 500 80' },
                labelElement: { xywh: '500 0 100 100' },
                contentElement: { xywh: '0 28 200 24' },
                errorElement: { xywh: '0 82 500 16' },
                errorIconElement: { xywh: '0 82 16 16' },
                errorMessageElement: { xywh: '21 82 479 16' },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                height: 100,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '0 24 600 56' },
                labelElement: { xywh: '0 0 600 24' },
                contentElement: { xywh: '0 40 200 24' },
                errorElement: { xywh: '0 82 600 16' },
                errorIconElement: { xywh: '0 82 16 16' },
                errorMessageElement: { xywh: '21 82 579 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                height: 100,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 100' },
                bodyElement: { xywh: '0 0 600 56' },
                labelElement: { xywh: '0 76 600 24' },
                contentElement: { xywh: '0 16 200 24' },
                errorElement: { xywh: '0 58 600 16' },
                errorIconElement: { xywh: '0 58 16 16' },
                errorMessageElement: { xywh: '21 58 579 16' },
                labelHtmlElement: { xywh: '0 80 50 17' }
            });
        });
    });

    describe('flex grow height', function() {
        var containerConfig = {
            height: 100,
            layout: {
                type: 'vbox',
                align: 'start'
            },
            defaults: {
                flex: 1
            }
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 326 100' },
                labelElement: { xywh: '0 0 100 100' },
                bodyElement: { xywh: '100 0 200 100' },
                contentElement: { xywh: '100 38 200 24' },
                errorElement: { xywh: '300 42 26 16' },
                errorIconElement: { xywh: '305 42 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 326 100' },
                labelElement: { xywh: '226 0 100 100' },
                bodyElement: { xywh: '26 0 200 100' },
                contentElement: { xywh: '26 38 200 24' },
                errorElement: { xywh: '0 42 26 16' },
                errorIconElement: { xywh: '5 42 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '232 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 226 100' },
                labelElement: { xywh: '0 0 226 24' },
                bodyElement: { xywh: '0 24 200 76' },
                contentElement: { xywh: '0 50 200 24' },
                errorElement: { xywh: '200 54 26 16' },
                errorIconElement: { xywh: '205 54 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 226 100' },
                labelElement: { xywh: '0 76 226 24' },
                bodyElement: { xywh: '0 0 200 76' },
                contentElement: { xywh: '0 26 200 24' },
                errorElement: { xywh: '200 30 26 16' },
                errorIconElement: { xywh: '205 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 80 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 100' },
                labelElement: { xywh: '0 0 100 100' },
                bodyElement: { xywh: '100 0 200 80' },
                contentElement: { xywh: '100 28 200 24' },
                errorElement: { xywh: '100 82 200 16' },
                errorIconElement: { xywh: '100 82 16 16' },
                errorMessageElement: { xywh: '121 82 179 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 100' },
                labelElement: { xywh: '200 0 100 100' },
                bodyElement: { xywh: '0 0 200 80' },
                contentElement: { xywh: '0 28 200 24' },
                errorElement: { xywh: '0 82 200 16' },
                errorIconElement: { xywh: '0 82 16 16' },
                errorMessageElement: { xywh: '21 82 179 16' },
                labelHtmlElement: { xywh: '206 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 200 100' },
                labelElement: { xywh: '0 0 200 24' },
                bodyElement: { xywh: '0 24 200 56' },
                contentElement: { xywh: '0 40 200 24' },
                errorElement: { xywh: '0 82 200 16' },
                errorIconElement: { xywh: '0 82 16 16' },
                errorMessageElement: { xywh: '21 82 179 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 200 100' },
                labelElement: { xywh: '0 76 200 24' },
                bodyElement: { xywh: '0 0 200 56' },
                contentElement: { xywh: '0 16 200 24' },
                errorElement: { xywh: '0 58 200 16' },
                errorIconElement: { xywh: '0 58 16 16' },
                errorMessageElement: { xywh: '21 58 179 16' },
                labelHtmlElement: { xywh: '0 80 50 17' }
            });
        });
    });

    describe('flex shrink height', function() {
        var containerConfig = {
            height: 10,
            layout: 'vbox',
            defaults: {
                flex: 1
            }
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 24'},
                bodyElement: {xywh: '100 0 474 24'},
                labelElement: {xywh: '0 0 100 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '574 4 26 16'},
                errorIconElement: {xywh: '579 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 24'},
                bodyElement: {xywh: '26 0 474 24'},
                labelElement: {xywh: '500 0 100 24'},
                contentElement: {xywh: '26 0 200 24'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '506 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 48'},
                bodyElement: {xywh: '0 24 574 24'},
                labelElement: {xywh: '0 0 600 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '574 28 26 16'},
                errorIconElement: {xywh: '579 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 48'},
                bodyElement: {xywh: '0 0 574 24'},
                labelElement: {xywh: '0 24 600 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '574 4 26 16'},
                errorIconElement: {xywh: '579 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 44'},
                bodyElement: {xywh: '100 0 500 24'},
                labelElement: {xywh: '0 0 100 44'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '100 26 500 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 479 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 44'},
                bodyElement: {xywh: '0 0 500 24'},
                labelElement: {xywh: '500 0 100 44'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 500 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 479 16'},
                labelHtmlElement: {xywh: '506 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 68'},
                bodyElement: {xywh: '0 24 600 24'},
                labelElement: {xywh: '0 0 600 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 600 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 579 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 600 68'},
                bodyElement: {xywh: '0 0 600 24'},
                labelElement: {xywh: '0 44 600 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 600 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 579 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('stretched width', function() {
        var containerConfig = {
            width: 400,
            layout: 'vbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 274 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '374 4 26 16'},
                errorIconElement: {xywh: '379 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 24'},
                labelElement: {xywh: '300 0 100 24'},
                bodyElement: {xywh: '26 0 274 24'},
                contentElement: {xywh: '26 0 200 24'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 48'},
                labelElement: {xywh: '0 0 400 24'},
                bodyElement: {xywh: '0 24 374 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '374 28 26 16'},
                errorIconElement: {xywh: '379 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 48'},
                labelElement: {xywh: '0 24 400 24'},
                bodyElement: {xywh: '0 0 374 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '374 4 26 16'},
                errorIconElement: {xywh: '379 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 300 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '100 26 300 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 279 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 44'},
                labelElement: {xywh: '300 0 100 44'},
                bodyElement: {xywh: '0 0 300 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 300 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 279 16'},
                labelHtmlElement: {xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 68'},
                labelElement: {xywh: '0 0 400 24'},
                bodyElement: {xywh: '0 24 400 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 400 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 379 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 400 68'},
                labelElement: {xywh: '0 44 400 24'},
                bodyElement: {xywh: '0 0 400 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 400 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 379 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });


    describe('stretched width - smaller than content width', function () {
        var containerConfig = {
            width: 150,
            layout: 'vbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 24 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '124 4 26 16'},
                errorIconElement: {xywh: '129 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 24'},
                labelElement: {xywh: '50 0 100 24'},
                bodyElement: {xywh: '26 0 24 24'},
                contentElement: {xywh: '26 0 200 24'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '56 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 48'},
                labelElement: {xywh: '0 0 150 24'},
                bodyElement: {xywh: '0 24 124 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '124 28 26 16'},
                errorIconElement: {xywh: '129 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 48'},
                labelElement: {xywh: '0 24 150 24'},
                bodyElement: {xywh: '0 0 124 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '124 4 26 16'},
                errorIconElement: {xywh: '129 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 50 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '100 26 50 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 29 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 44'},
                labelElement: {xywh: '50 0 100 44'},
                bodyElement: {xywh: '0 0 50 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 50 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 29 16'},
                labelHtmlElement: {xywh: '56 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 68'},
                labelElement: {xywh: '0 0 150 24'},
                bodyElement: {xywh: '0 24 150 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 150 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 129 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 150 68'},
                labelElement: {xywh: '0 44 150 24'},
                bodyElement: {xywh: '0 0 150 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 150 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 129 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('stretched width - smaller than label width', function () {
        var containerConfig = {
            width: 50,
            layout: 'vbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 24'},
                labelElement: {xywh: '0 0 50 24'},
                bodyWrapElement: {xywh: '50 0 0 24'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 24'},
                labelElement: {xywh: '0 0 50 24'},
                bodyWrapElement: {xywh: '0 0 0 24'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 24 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '24 28 26 16'},
                errorIconElement: {xywh: '29 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 24 50 24'},
                bodyElement: {xywh: '0 0 24 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '24 4 26 16'},
                errorIconElement: {xywh: '29 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 44'},
                labelElement: {xywh: '0 0 50 44'},
                bodyWrapElement: {xywh: '50 0 0 44'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 44'},
                labelElement: {xywh: '0 0 50 44'},
                bodyWrapElement: {xywh: '0 0 0 44'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 50 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 50 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 29 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 44 50 24'},
                bodyElement: {xywh: '0 0 50 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 50 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 29 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('stretched height', function() {
        var containerConfig = {
            height: 100,
            layout: 'hbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 326 100'},
                labelElement: {xywh: '0 0 100 100'},
                bodyElement: {xywh: '100 0 200 100'},
                contentElement: {xywh: '100 38 200 24'},
                errorElement: {xywh: '300 42 26 16'},
                errorIconElement: {xywh: '305 42 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 326 100'},
                labelElement: {xywh: '226 0 100 100'},
                bodyElement: {xywh: '26 0 200 100'},
                contentElement: {xywh: '26 38 200 24'},
                errorElement: {xywh: '0 42 26 16'},
                errorIconElement: {xywh: '5 42 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '232 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 226 100'},
                labelElement: {xywh: '0 0 226 24'},
                bodyElement: {xywh: '0 24 200 76'},
                contentElement: {xywh: '0 50 200 24'},
                errorElement: {xywh: '200 54 26 16'},
                errorIconElement: {xywh: '205 54 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 226 100'},
                labelElement: {xywh: '0 76 226 24'},
                bodyElement: {xywh: '0 0 200 76'},
                contentElement: {xywh: '0 26 200 24'},
                errorElement: {xywh: '200 30 26 16'},
                errorIconElement: {xywh: '205 30 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 80 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 300 100'},
                labelElement: {xywh: '0 0 100 100'},
                bodyElement: {xywh: '100 0 200 80'},
                contentElement: {xywh: '100 28 200 24'},
                errorElement: {xywh: '100 82 200 16'},
                errorIconElement: {xywh: '100 82 16 16'},
                errorMessageElement: {xywh: '121 82 179 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 300 100'},
                labelElement: {xywh: '200 0 100 100'},
                bodyElement: {xywh: '0 0 200 80'},
                contentElement: {xywh: '0 28 200 24'},
                errorElement: {xywh: '0 82 200 16'},
                errorIconElement: {xywh: '0 82 16 16'},
                errorMessageElement: {xywh: '21 82 179 16'},
                labelHtmlElement: {xywh: '206 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 200 100'},
                labelElement: {xywh: '0 0 200 24'},
                bodyElement: {xywh: '0 24 200 56'},
                contentElement: {xywh: '0 40 200 24'},
                errorElement: {xywh: '0 82 200 16'},
                errorIconElement: {xywh: '0 82 16 16'},
                errorMessageElement: {xywh: '21 82 179 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 200 100'},
                labelElement: {xywh: '0 76 200 24'},
                bodyElement: {xywh: '0 0 200 56'},
                contentElement: {xywh: '0 16 200 24'},
                errorElement: {xywh: '0 58 200 16'},
                errorIconElement: {xywh: '0 58 16 16'},
                errorMessageElement: {xywh: '21 58 179 16'},
                labelHtmlElement: {xywh: '0 80 50 17'}
            });
        });
    });

    (Ext.supports.CSSMinContent ? describe : xdescribe)('stretched height - smaller than content height', function () {
        var containerConfig = {
            height: 10,
            layout: 'hbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 326 24' },
                labelElement: { xywh: '0 0 100 24' },
                bodyElement: { xywh: '100 0 200 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '300 4 26 16' },
                errorIconElement: { xywh: '305 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 326 24' },
                labelElement: { xywh: '226 0 100 24' },
                bodyElement: { xywh: '26 0 200 24' },
                contentElement: { xywh: '26 0 200 24' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '232 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 226 48' },
                labelElement: { xywh: '0 0 226 24' },
                bodyElement: { xywh: '0 24 200 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '200 28 26 16' },
                errorIconElement: { xywh: '205 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 226 48' },
                labelElement: { xywh: '0 24 226 24' },
                bodyElement: { xywh: '0 0 200 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '200 4 26 16' },
                errorIconElement: { xywh: '205 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 44' },
                labelElement: { xywh: '0 0 100 44' },
                bodyElement: { xywh: '100 0 200 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '100 26 200 16' },
                errorIconElement: { xywh: '100 26 16 16' },
                errorMessageElement: { xywh: '121 26 179 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 44' },
                labelElement: { xywh: '200 0 100 44' },
                bodyElement: { xywh: '0 0 200 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 200 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 179 16' },
                labelHtmlElement: { xywh: '206 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 200 68' },
                labelElement: { xywh: '0 0 200 24' },
                bodyElement: { xywh: '0 24 200 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '0 50 200 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 179 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 200 68' },
                labelElement: { xywh: '0 44 200 24' },
                bodyElement: { xywh: '0 0 200 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 200 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 179 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('inline', function() {
        it('should layout with left label and side error', function () {
            create({
                inline: true,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 326 24' },
                labelElement: { xywh: '0 0 100 24' },
                bodyElement: { xywh: '100 0 200 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '300 4 26 16' },
                errorIconElement: { xywh: '305 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                inline: true,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 326 24' },
                labelElement: { xywh: '226 0 100 24' },
                bodyElement: { xywh: '26 0 200 24' },
                contentElement: { xywh: '26 0 200 24' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '232 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                inline: true,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 226 48' },
                labelElement: { xywh: '0 0 226 24' },
                bodyElement: { xywh: '0 24 200 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '200 28 26 16' },
                errorIconElement: { xywh: '205 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                inline: true,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 226 48' },
                labelElement: { xywh: '0 24 226 24' },
                bodyElement: { xywh: '0 0 200 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '200 4 26 16' },
                errorIconElement: { xywh: '205 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                inline: true,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 44' },
                labelElement: { xywh: '0 0 100 44' },
                bodyElement: { xywh: '100 0 200 24' },
                contentElement: { xywh: '100 0 200 24' },
                errorElement: { xywh: '100 26 200 16' },
                errorIconElement: { xywh: '100 26 16 16' },
                errorMessageElement: { xywh: '121 26 179 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                inline: true,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 44' },
                labelElement: { xywh: '200 0 100 44' },
                bodyElement: { xywh: '0 0 200 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 200 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 179 16' },
                labelHtmlElement: { xywh: '206 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                inline: true,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 200 68' },
                labelElement: { xywh: '0 0 200 24' },
                bodyElement: { xywh: '0 24 200 24' },
                contentElement: { xywh: '0 24 200 24' },
                errorElement: { xywh: '0 50 200 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 179 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                inline: true,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 200 68' },
                labelElement: { xywh: '0 44 200 24' },
                bodyElement: { xywh: '0 0 200 24' },
                contentElement: { xywh: '0 0 200 24' },
                errorElement: { xywh: '0 26 200 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 179 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('floated', function() {
        it('should layout with left label and side error', function () {
            create({
                floated: true,
                labelAlign: 'left',
                errorTarget: 'side'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 326 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 200 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '300 4 26 16'},
                errorIconElement: {xywh: '305 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                floated: true,
                labelAlign: 'right',
                errorTarget: 'side'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 326 24'},
                labelElement: {xywh: '226 0 100 24'},
                bodyElement: {xywh: '26 0 200 24'},
                contentElement: {xywh: '26 0 200 24'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '232 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                floated: true,
                labelAlign: 'top',
                errorTarget: 'side'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 226 48'},
                labelElement: {xywh: '0 0 226 24'},
                bodyElement: {xywh: '0 24 200 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '200 28 26 16'},
                errorIconElement: {xywh: '205 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                floated: true,
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 226 48'},
                labelElement: {xywh: '0 24 226 24'},
                bodyElement: {xywh: '0 0 200 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '200 4 26 16'},
                errorIconElement: {xywh: '205 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                floated: true,
                labelAlign: 'left',
                errorTarget: 'under'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 300 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 200 24'},
                contentElement: {xywh: '100 0 200 24'},
                errorElement: {xywh: '100 26 200 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 179 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                floated: true,
                labelAlign: 'right',
                errorTarget: 'under'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 300 44'},
                labelElement: {xywh: '200 0 100 44'},
                bodyElement: {xywh: '0 0 200 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 200 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 179 16'},
                labelHtmlElement: {xywh: '206 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                floated: true,
                labelAlign: 'top',
                errorTarget: 'under'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 200 68'},
                labelElement: {xywh: '0 0 200 24'},
                bodyElement: {xywh: '0 24 200 24'},
                contentElement: {xywh: '0 24 200 24'},
                errorElement: {xywh: '0 50 200 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 179 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                floated: true,
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, null);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 200 68'},
                labelElement: {xywh: '0 44 200 24'},
                bodyElement: {xywh: '0 0 200 24'},
                contentElement: {xywh: '0 0 200 24'},
                errorElement: {xywh: '0 26 200 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 179 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    // TODO: test labelWidth 'auto'
});























































