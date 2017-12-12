describe('Ext.field.Text_layout', function () {
    var labelHtml = '<span style="display:inline-block;width:50px;background:green;">&nbsp;</span>',
        container, field;

    function create(config, containerConfig) {
        field = new Ext.field.Text(Ext.apply({
            label: labelHtml,
            triggers: {
                a: { side: 'left' },
                b: { side: 'left' },
                c: { side: 'right' },
                d: { side: 'right'}
            }
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

    afterEach(function () {
        if (container) {
            container.destroy();
            container = null;
        }

        if (field) {
            field.destroy();
            field = null;
        }
    });

    describe('auto size', function () {
        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '100 0 474 24' },
                labelElement: { xywh: '0 0 100 24' },
                inputElement: { xywh: '145 1 384 22' },
                beforeInputElement: { xywh: '101 1 44 22' },
                afterInputElement: { xywh: '529 1 44 22' },
                errorElement: { xywh: '574 4 26 16' },
                errorIconElement: { xywh: '579 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '26 0 474 24' },
                labelElement: { xywh: '500 0 100 24' },
                inputElement: { xywh: '71 1 384 22' },
                beforeInputElement: { xywh: '27 1 44 22' },
                afterInputElement: { xywh: '455 1 44 22' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 48' },
                bodyElement: { xywh: '0 24 574 24' },
                labelElement: { xywh: '0 0 600 24' },
                inputElement: { xywh: '45 25 484 22' },
                beforeInputElement: { xywh: '1 25 44 22' },
                afterInputElement: { xywh: '529 25 44 22' },
                errorElement: { xywh: '574 28 26 16' },
                errorIconElement: { xywh: '579 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 48' },
                bodyElement: { xywh: '0 0 574 24' },
                labelElement: { xywh: '0 24 600 24' },
                inputElement: { xywh: '45 1 484 22' },
                beforeInputElement: { xywh: '1 1 44 22' },
                afterInputElement: { xywh: '529 1 44 22' },
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
                inputElement: { xywh: '145 1 410 22' },
                beforeInputElement: { xywh: '101 1 44 22' },
                afterInputElement: { xywh: '555 1 44 22' },
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
                inputElement: { xywh: '45 1 410 22' },
                beforeInputElement: { xywh: '1 1 44 22' },
                afterInputElement: { xywh: '455 1 44 22' },
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
                inputElement: { xywh: '45 25 510 22' },
                beforeInputElement: { xywh: '1 25 44 22' },
                afterInputElement: { xywh: '555 25 44 22' },
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
                inputElement: { xywh: '45 1 510 22' },
                beforeInputElement: { xywh: '1 1 44 22' },
                afterInputElement: { xywh: '555 1 44 22' },
                errorElement: { xywh: '0 26 600 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 579 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('configured width - larger than default width', function () {
        it('should layout with left label and side error', function () {
            create({
                width: 400,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '100 0 274 24' },
                labelElement: { xywh: '0 0 100 24' },
                inputElement: { xywh: '145 1 184 22' },
                beforeInputElement: { xywh: '101 1 44 22' },
                afterInputElement: { xywh: '329 1 44 22' },
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
                bodyElement: { xywh: '26 0 274 24' },
                labelElement: { xywh: '300 0 100 24' },
                inputElement: { xywh: '71 1 184 22' },
                beforeInputElement: { xywh: '27 1 44 22' },
                afterInputElement: { xywh: '255 1 44 22' },
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
                bodyElement: { xywh: '0 24 374 24' },
                labelElement: { xywh: '0 0 400 24' },
                inputElement: { xywh: '45 25 284 22' },
                beforeInputElement: { xywh: '1 25 44 22' },
                afterInputElement: { xywh: '329 25 44 22' },
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
                bodyElement: { xywh: '0 0 374 24' },
                labelElement: { xywh: '0 24 400 24' },
                inputElement: { xywh: '45 1 284 22' },
                beforeInputElement: { xywh: '1 1 44 22' },
                afterInputElement: { xywh: '329 1 44 22' },
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
                bodyElement: { xywh: '100 0 300 24' },
                labelElement: { xywh: '0 0 100 44' },
                inputElement: { xywh: '145 1 210 22' },
                beforeInputElement: { xywh: '101 1 44 22' },
                afterInputElement: { xywh: '355 1 44 22' },
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
                bodyElement: { xywh: '0 0 300 24' },
                labelElement: { xywh: '300 0 100 44' },
                inputElement: { xywh: '45 1 210 22' },
                beforeInputElement: { xywh: '1 1 44 22' },
                afterInputElement: { xywh: '255 1 44 22' },
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
                bodyElement: { xywh: '0 24 400 24' },
                labelElement: { xywh: '0 0 400 24' },
                inputElement: { xywh: '45 25 310 22' },
                beforeInputElement: { xywh: '1 25 44 22' },
                afterInputElement: { xywh: '355 25 44 22' },
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
                bodyElement: { xywh: '0 0 400 24' },
                labelElement: { xywh: '0 44 400 24' },
                inputElement: { xywh: '45 1 310 22' },
                beforeInputElement: { xywh: '1 1 44 22' },
                afterInputElement: { xywh: '355 1 44 22' },
                errorElement: { xywh: '0 26 400 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 379 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('configured width - smaller than default width', function () {
        it('should layout with left label and side error', function () {
            create({
                width: 240,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 24' },
                labelElement: { xywh: '0 0 100 24' },
                bodyElement: { xywh: '100 0 114 24' },
                beforeInputElement: { xywh: '101 1 44 22' },
                inputElement: { xywh: '145 1 24 22' },
                afterInputElement: { xywh: '169 1 44 22' },
                errorElement: { xywh: '214 4 26 16' },
                errorIconElement: { xywh: '219 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                width: 240,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 24' },
                labelElement: { xywh: '140 0 100 24' },
                bodyElement: { xywh: '26 0 114 24' },
                beforeInputElement: { xywh: '27 1 44 22' },
                inputElement: { xywh: '71 1 24 22' },
                afterInputElement: { xywh: '95 1 44 22' },
                errorElement: { xywh: '0 4 26 16' },
                errorIconElement: { xywh: '5 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '146 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                width: 140,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 140 48' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 114 24' },
                beforeInputElement: { xywh: '1 25 44 22' },
                inputElement: { xywh: '45 25 24 22' },
                afterInputElement: { xywh: '69 25 44 22' },
                errorElement: { xywh: '114 28 26 16' },
                errorIconElement: { xywh: '119 28 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                width: 140,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 140 48' },
                labelElement: { xywh: '0 24 140 24' },
                bodyElement: { xywh: '0 0 114 24' },
                beforeInputElement: { xywh: '1 1 44 22' },
                inputElement: { xywh: '45 1 24 22' },
                afterInputElement: { xywh: '69 1 44 22' },
                errorElement: { xywh: '114 4 26 16' },
                errorIconElement: { xywh: '119 4 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 28 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                width: 240,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 44' },
                labelElement: { xywh: '0 0 100 44' },
                bodyElement: { xywh: '100 0 140 24' },
                beforeInputElement: { xywh: '101 1 44 22' },
                inputElement: { xywh: '145 1 50 22' },
                afterInputElement: { xywh: '195 1 44 22' },
                errorElement: { xywh: '100 26 140 16' },
                errorIconElement: { xywh: '100 26 16 16' },
                errorMessageElement: { xywh: '121 26 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                width: 240,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 44' },
                labelElement: { xywh: '140 0 100 44' },
                bodyElement: { xywh: '0 0 140 24' },
                beforeInputElement: { xywh: '1 1 44 22' },
                inputElement: { xywh: '45 1 50 22' },
                afterInputElement: { xywh: '95 1 44 22' },
                errorElement: { xywh: '0 26 140 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 119 16' },
                labelHtmlElement: { xywh: '146 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                width: 140,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 140 68' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 140 24' },
                beforeInputElement: { xywh: '1 25 44 22' },
                inputElement: { xywh: '45 25 50 22' },
                afterInputElement: { xywh: '95 25 44 22' },
                errorElement: { xywh: '0 50 140 16' },
                errorIconElement: { xywh: '0 50 16 16' },
                errorMessageElement: { xywh: '21 50 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                width: 140,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 140 68' },
                labelElement: { xywh: '0 44 140 24' },
                bodyElement: { xywh: '0 0 140 24' },
                beforeInputElement: { xywh: '1 1 44 22' },
                inputElement: { xywh: '45 1 50 22' },
                afterInputElement: { xywh: '95 1 44 22' },
                errorElement: { xywh: '0 26 140 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 119 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('configured width - smaller than label width', function () {
        it('should layout with left label and side error', function () {
            create({
                width: 50,
                labelAlign: 'left',
                errorTarget: 'side',
                triggers: null
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
                errorTarget: 'side',
                triggers: null
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
                errorTarget: 'side',
                triggers: null
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 24 24'},
                beforeInputElement: {xywh: '1 25 0 22'},
                inputElement: {xywh: '1 25 22 22'},
                afterInputElement: {xywh: '23 25 0 22'},
                errorElement: {xywh: '24 28 26 16'},
                errorIconElement: {xywh: '29 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                width: 50,
                labelAlign: 'bottom',
                errorTarget: 'side',
                triggers: null
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 48' },
                labelElement: { xywh: '0 24 50 24' },
                bodyElement: { xywh: '0 0 24 24' },
                beforeInputElement: { xywh: '1 1 0 22' },
                inputElement: { xywh: '1 1 22 22' },
                afterInputElement: { xywh: '23 1 0 22' },
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
                errorTarget: 'under',
                triggers: null
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
                errorTarget: 'under',
                triggers: null
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
                errorTarget: 'under',
                triggers: null
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 68' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 50 24' },
                beforeInputElement: { xywh: '1 25 0 22' },
                inputElement: { xywh: '1 25 48 22' },
                afterInputElement: { xywh: '49 25 0 22' },
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
                errorTarget: 'under',
                triggers: null
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 68' },
                labelElement: { xywh: '0 44 50 24' },
                bodyElement: { xywh: '0 0 50 24' },
                beforeInputElement: { xywh: '1 1 0 22' },
                inputElement: { xywh: '1 1 48 22' },
                afterInputElement: { xywh: '49 1 0 22' },
                errorElement: { xywh: '0 26 50 16' },
                errorIconElement: { xywh: '0 26 16 16' },
                errorMessageElement: { xywh: '21 26 29 16' },
                labelHtmlElement: { xywh: '0 48 50 17' }
            });
        });
    });

    describe('flex grow width', function () {
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
                bodyElement: {xywh: '100 0 274 24'},
                labelElement: {xywh: '0 0 100 24'},
                inputElement: {xywh: '145 1 184 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '329 1 44 22'},
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
                bodyElement: {xywh: '26 0 274 24'},
                labelElement: {xywh: '300 0 100 24'},
                inputElement: {xywh: '71 1 184 22'},
                beforeInputElement: {xywh: '27 1 44 22'},
                afterInputElement: {xywh: '255 1 44 22'},
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
                bodyElement: {xywh: '0 24 374 24'},
                labelElement: {xywh: '0 0 400 24'},
                inputElement: {xywh: '45 25 284 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '329 25 44 22'},
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
                bodyElement: {xywh: '0 0 374 24'},
                labelElement: {xywh: '0 24 400 24'},
                inputElement: {xywh: '45 1 284 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '329 1 44 22'},
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
                bodyElement: {xywh: '100 0 300 24'},
                labelElement: {xywh: '0 0 100 44'},
                inputElement: {xywh: '145 1 210 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '355 1 44 22'},
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
                bodyElement: {xywh: '0 0 300 24'},
                labelElement: {xywh: '300 0 100 44'},
                inputElement: {xywh: '45 1 210 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '255 1 44 22'},
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
                bodyElement: {xywh: '0 24 400 24'},
                labelElement: {xywh: '0 0 400 24'},
                inputElement: {xywh: '45 25 310 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '355 25 44 22'},
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
                bodyElement: {xywh: '0 0 400 24'},
                labelElement: {xywh: '0 44 400 24'},
                inputElement: {xywh: '45 1 310 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '355 1 44 22'},
                errorElement: {xywh: '0 26 400 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 379 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('flex shrink width', function () {
        var containerConfig;

        beforeEach(function() {
            containerConfig = {
                width: 240,
                layout: 'hbox',
                defaults: {
                    flex: 1
                }
            };
        });

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 240 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 114 24'},
                beforeInputElement: {xywh: '101 1 44 22'},
                inputElement: {xywh: '145 1 24 22'},
                afterInputElement: {xywh: '169 1 44 22'},
                errorElement: {xywh: '214 4 26 16'},
                errorIconElement: {xywh: '219 4 16 16'},
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
                element: {xywh: '0 0 240 24'},
                labelElement: {xywh: '140 0 100 24'},
                bodyElement: {xywh: '26 0 114 24'},
                beforeInputElement: {xywh: '27 1 44 22'},
                inputElement: {xywh: '71 1 24 22'},
                afterInputElement: {xywh: '95 1 44 22'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '146 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 48'},
                labelElement: {xywh: '0 0 140 24'},
                bodyElement: {xywh: '0 24 114 24'},
                beforeInputElement: {xywh: '1 25 44 22'},
                inputElement: {xywh: '45 25 24 22'},
                afterInputElement: {xywh: '69 25 44 22'},
                errorElement: {xywh: '114 28 26 16'},
                errorIconElement: {xywh: '119 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 48'},
                labelElement: {xywh: '0 24 140 24'},
                bodyElement: {xywh: '0 0 114 24'},
                beforeInputElement: {xywh: '1 1 44 22'},
                inputElement: {xywh: '45 1 24 22'},
                afterInputElement: {xywh: '69 1 44 22'},
                errorElement: {xywh: '114 4 26 16'},
                errorIconElement: {xywh: '119 4 16 16'},
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
                element: {xywh: '0 0 240 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 140 24'},
                beforeInputElement: {xywh: '101 1 44 22'},
                inputElement: {xywh: '145 1 50 22'},
                afterInputElement: {xywh: '195 1 44 22'},
                errorElement: {xywh: '100 26 140 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 119 16'},
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
                element: {xywh: '0 0 240 44'},
                labelElement: {xywh: '140 0 100 44'},
                bodyElement: {xywh: '0 0 140 24'},
                beforeInputElement: {xywh: '1 1 44 22'},
                inputElement: {xywh: '45 1 50 22'},
                afterInputElement: {xywh: '95 1 44 22'},
                errorElement: {xywh: '0 26 140 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 119 16'},
                labelHtmlElement: {xywh: '146 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 68'},
                labelElement: {xywh: '0 0 140 24'},
                bodyElement: {xywh: '0 24 140 24'},
                beforeInputElement: {xywh: '1 25 44 22'},
                inputElement: {xywh: '45 25 50 22'},
                afterInputElement: {xywh: '95 25 44 22'},
                errorElement: {xywh: '0 50 140 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 119 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 68'},
                labelElement: {xywh: '0 44 140 24'},
                bodyElement: {xywh: '0 0 140 24'},
                beforeInputElement: {xywh: '1 1 44 22'},
                inputElement: {xywh: '45 1 50 22'},
                afterInputElement: {xywh: '95 1 44 22'},
                errorElement: {xywh: '0 26 140 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 119 16'},
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
                errorTarget: 'side',
                triggers: null
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
                errorTarget: 'side',
                triggers: null
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
                errorTarget: 'side',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 24 24'},
                beforeInputElement: {xywh: '1 25 0 22'},
                inputElement: {xywh: '1 25 22 22'},
                afterInputElement: {xywh: '23 25 0 22'},
                errorElement: {xywh: '24 28 26 16'},
                errorIconElement: {xywh: '29 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 24 50 24'},
                bodyElement: {xywh: '0 0 24 24'},
                beforeInputElement: {xywh: '1 1 0 22'},
                inputElement: {xywh: '1 1 22 22'},
                afterInputElement: {xywh: '23 1 0 22'},
                errorElement: {xywh: '24 4 26 16'},
                errorIconElement: {xywh: '29 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under',
                triggers: null
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
                errorTarget: 'under',
                triggers: null
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
                errorTarget: 'under',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 50 24'},
                beforeInputElement: {xywh: '1 25 0 22'},
                inputElement: {xywh: '1 25 48 22'},
                afterInputElement: {xywh: '49 25 0 22'},
                errorElement: {xywh: '0 50 50 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 29 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 44 50 24'},
                bodyElement: {xywh: '0 0 50 24'},
                beforeInputElement: {xywh: '1 1 0 22'},
                inputElement: {xywh: '1 1 48 22'},
                afterInputElement: {xywh: '49 1 0 22'},
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
                inputElement: { xywh: '145 1 384 98' },
                beforeInputElement: { xywh: '101 1 44 98' },
                afterInputElement: { xywh: '529 1 44 98' },
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
                inputElement: { xywh: '71 1 384 98' },
                beforeInputElement: { xywh: '27 1 44 98' },
                afterInputElement: { xywh: '455 1 44 98' },
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
                inputElement: { xywh: '45 25 484 74' },
                beforeInputElement: { xywh: '1 25 44 74' },
                afterInputElement: { xywh: '529 25 44 74' },
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
                inputElement: { xywh: '45 1 484 74' },
                beforeInputElement: { xywh: '1 1 44 74' },
                afterInputElement: { xywh: '529 1 44 74' },
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
                inputElement: { xywh: '145 1 410 78' },
                beforeInputElement: { xywh: '101 1 44 78' },
                afterInputElement: { xywh: '555 1 44 78' },
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
                inputElement: { xywh: '45 1 410 78' },
                beforeInputElement: { xywh: '1 1 44 78' },
                afterInputElement: { xywh: '455 1 44 78' },
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
                inputElement: { xywh: '45 25 510 54' },
                beforeInputElement: { xywh: '1 25 44 54' },
                afterInputElement: { xywh: '555 25 44 54' },
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
                inputElement: { xywh: '45 1 510 54' },
                beforeInputElement: { xywh: '1 1 44 54' },
                afterInputElement: { xywh: '555 1 44 54' },
                errorElement: { xywh: '0 58 600 16' },
                errorIconElement: { xywh: '0 58 16 16' },
                errorMessageElement: { xywh: '21 58 579 16' },
                labelHtmlElement: { xywh: '0 80 50 17' }
            });
        });
    });

    describe('flex grow height', function () {
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
                element: { xywh: '0 0 270 100' },
                bodyElement: { xywh: '100 0 144 100' },
                labelElement: { xywh: '0 0 100 100' },
                inputElement: { xywh: '145 1 54 98' },
                beforeInputElement: { xywh: '101 1 44 98' },
                afterInputElement: { xywh: '199 1 44 98' },
                errorElement: { xywh: '244 42 26 16' },
                errorIconElement: { xywh: '249 42 16 16' },
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
                element: { xywh: '0 0 270 100' },
                bodyElement: { xywh: '26 0 144 100' },
                labelElement: { xywh: '170 0 100 100' },
                inputElement: { xywh: '71 1 54 98' },
                beforeInputElement: { xywh: '27 1 44 98' },
                afterInputElement: { xywh: '125 1 44 98' },
                errorElement: { xywh: '0 42 26 16' },
                errorIconElement: { xywh: '5 42 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '176 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 100' },
                bodyElement: { xywh: '0 24 144 76' },
                labelElement: { xywh: '0 0 170 24' },
                inputElement: { xywh: '45 25 54 74' },
                beforeInputElement: { xywh: '1 25 44 74' },
                afterInputElement: { xywh: '99 25 44 74' },
                errorElement: { xywh: '144 54 26 16' },
                errorIconElement: { xywh: '149 54 16 16' },
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
                element: { xywh: '0 0 170 100' },
                bodyElement: { xywh: '0 0 144 76' },
                labelElement: { xywh: '0 76 170 24' },
                inputElement: { xywh: '45 1 54 74' },
                beforeInputElement: { xywh: '1 1 44 74' },
                afterInputElement: { xywh: '99 1 44 74' },
                errorElement: { xywh: '144 30 26 16' },
                errorIconElement: { xywh: '149 30 16 16' },
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
                element: { xywh: '0 0 270 100' },
                bodyElement: { xywh: '100 0 170 80' },
                labelElement: { xywh: '0 0 100 100' },
                inputElement: { xywh: '145 1 80 78' },
                beforeInputElement: { xywh: '101 1 44 78' },
                afterInputElement: { xywh: '225 1 44 78' },
                errorElement: { xywh: '100 82 170 16' },
                errorIconElement: { xywh: '100 82 16 16' },
                errorMessageElement: { xywh: '121 82 149 16' },
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
                element: { xywh: '0 0 270 100' },
                bodyElement: { xywh: '0 0 170 80' },
                labelElement: { xywh: '170 0 100 100' },
                inputElement: { xywh: '45 1 80 78' },
                beforeInputElement: { xywh: '1 1 44 78' },
                afterInputElement: { xywh: '125 1 44 78' },
                errorElement: { xywh: '0 82 170 16' },
                errorIconElement: { xywh: '0 82 16 16' },
                errorMessageElement: { xywh: '21 82 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17' }
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 100' },
                bodyElement: { xywh: '0 24 170 56' },
                labelElement: { xywh: '0 0 170 24' },
                inputElement: { xywh: '45 25 80 54' },
                beforeInputElement: { xywh: '1 25 44 54' },
                afterInputElement: { xywh: '125 25 44 54' },
                errorElement: { xywh: '0 82 170 16' },
                errorIconElement: { xywh: '0 82 16 16' },
                errorMessageElement: { xywh: '21 82 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 100' },
                bodyElement: { xywh: '0 0 170 56' },
                labelElement: { xywh: '0 76 170 24' },
                inputElement: { xywh: '45 1 80 54' },
                beforeInputElement: { xywh: '1 1 44 54' },
                afterInputElement: { xywh: '125 1 44 54' },
                errorElement: { xywh: '0 58 170 16' },
                errorIconElement: { xywh: '0 58 16 16' },
                errorMessageElement: { xywh: '21 58 149 16' },
                labelHtmlElement: { xywh: '0 80 50 17' }
            });
        });
    });

    describe('flex shrink height', function () {
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
                inputElement: {xywh: '145 1 384 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '529 1 44 22'},
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
                inputElement: {xywh: '71 1 384 22'},
                beforeInputElement: {xywh: '27 1 44 22'},
                afterInputElement: {xywh: '455 1 44 22'},
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
                inputElement: {xywh: '45 25 484 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '529 25 44 22'},
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
                inputElement: {xywh: '45 1 484 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '529 1 44 22'},
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
                inputElement: {xywh: '145 1 410 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '555 1 44 22'},
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
                inputElement: {xywh: '45 1 410 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '455 1 44 22'},
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
                inputElement: {xywh: '45 25 510 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '555 25 44 22'},
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
                inputElement: {xywh: '45 1 510 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '555 1 44 22'},
                errorElement: {xywh: '0 26 600 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 579 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('stretched width', function () {
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
                bodyElement: {xywh: '100 0 274 24'},
                labelElement: {xywh: '0 0 100 24'},
                inputElement: {xywh: '145 1 184 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '329 1 44 22'},
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
                bodyElement: {xywh: '26 0 274 24'},
                labelElement: {xywh: '300 0 100 24'},
                inputElement: {xywh: '71 1 184 22'},
                beforeInputElement: {xywh: '27 1 44 22'},
                afterInputElement: {xywh: '255 1 44 22'},
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
                bodyElement: {xywh: '0 24 374 24'},
                labelElement: {xywh: '0 0 400 24'},
                inputElement: {xywh: '45 25 284 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '329 25 44 22'},
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
                bodyElement: {xywh: '0 0 374 24'},
                labelElement: {xywh: '0 24 400 24'},
                inputElement: {xywh: '45 1 284 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '329 1 44 22'},
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
                bodyElement: {xywh: '100 0 300 24'},
                labelElement: {xywh: '0 0 100 44'},
                inputElement: {xywh: '145 1 210 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '355 1 44 22'},
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
                bodyElement: {xywh: '0 0 300 24'},
                labelElement: {xywh: '300 0 100 44'},
                inputElement: {xywh: '45 1 210 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '255 1 44 22'},
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
                bodyElement: {xywh: '0 24 400 24'},
                labelElement: {xywh: '0 0 400 24'},
                inputElement: {xywh: '45 25 310 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '355 25 44 22'},
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
                bodyElement: {xywh: '0 0 400 24'},
                labelElement: {xywh: '0 44 400 24'},
                inputElement: {xywh: '45 1 310 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '355 1 44 22'},
                errorElement: {xywh: '0 26 400 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 379 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('stretched width - smaller than default width', function () {
        var containerConfig;

        beforeEach(function() {
            containerConfig = {
                width: 240,
                layout: 'vbox'
            }
        });

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 240 24'},
                labelElement: {xywh: '0 0 100 24'},
                bodyElement: {xywh: '100 0 114 24'},
                beforeInputElement: {xywh: '101 1 44 22'},
                inputElement: {xywh: '145 1 24 22'},
                afterInputElement: {xywh: '169 1 44 22'},
                errorElement: {xywh: '214 4 26 16'},
                errorIconElement: {xywh: '219 4 16 16'},
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
                element: {xywh: '0 0 240 24'},
                labelElement: {xywh: '140 0 100 24'},
                bodyElement: {xywh: '26 0 114 24'},
                beforeInputElement: {xywh: '27 1 44 22'},
                inputElement: {xywh: '71 1 24 22'},
                afterInputElement: {xywh: '95 1 44 22'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '146 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 48'},
                labelElement: {xywh: '0 0 140 24'},
                bodyElement: {xywh: '0 24 114 24'},
                beforeInputElement: {xywh: '1 25 44 22'},
                inputElement: {xywh: '45 25 24 22'},
                afterInputElement: {xywh: '69 25 44 22'},
                errorElement: {xywh: '114 28 26 16'},
                errorIconElement: {xywh: '119 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 48'},
                labelElement: {xywh: '0 24 140 24'},
                bodyElement: {xywh: '0 0 114 24'},
                beforeInputElement: {xywh: '1 1 44 22'},
                inputElement: {xywh: '45 1 24 22'},
                afterInputElement: {xywh: '69 1 44 22'},
                errorElement: {xywh: '114 4 26 16'},
                errorIconElement: {xywh: '119 4 16 16'},
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
                element: {xywh: '0 0 240 44'},
                labelElement: {xywh: '0 0 100 44'},
                bodyElement: {xywh: '100 0 140 24'},
                beforeInputElement: {xywh: '101 1 44 22'},
                inputElement: {xywh: '145 1 50 22'},
                afterInputElement: {xywh: '195 1 44 22'},
                errorElement: {xywh: '100 26 140 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 119 16'},
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
                element: {xywh: '0 0 240 44'},
                labelElement: {xywh: '140 0 100 44'},
                bodyElement: {xywh: '0 0 140 24'},
                beforeInputElement: {xywh: '1 1 44 22'},
                inputElement: {xywh: '45 1 50 22'},
                afterInputElement: {xywh: '95 1 44 22'},
                errorElement: {xywh: '0 26 140 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 119 16'},
                labelHtmlElement: {xywh: '146 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 68'},
                labelElement: {xywh: '0 0 140 24'},
                bodyElement: {xywh: '0 24 140 24'},
                beforeInputElement: {xywh: '1 25 44 22'},
                inputElement: {xywh: '45 25 50 22'},
                afterInputElement: {xywh: '95 25 44 22'},
                errorElement: {xywh: '0 50 140 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 119 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            containerConfig.width = 140;

            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 140 68'},
                labelElement: {xywh: '0 44 140 24'},
                bodyElement: {xywh: '0 0 140 24'},
                beforeInputElement: {xywh: '1 1 44 22'},
                inputElement: {xywh: '45 1 50 22'},
                afterInputElement: {xywh: '95 1 44 22'},
                errorElement: {xywh: '0 26 140 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 119 16'},
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
                errorTarget: 'side',
                triggers: null
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
                errorTarget: 'side',
                triggers: null
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
                errorTarget: 'side',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 24 24'},
                beforeInputElement: {xywh: '1 25 0 22'},
                inputElement: {xywh: '1 25 22 22'},
                afterInputElement: {xywh: '23 25 0 22'},
                errorElement: {xywh: '24 28 26 16'},
                errorIconElement: {xywh: '29 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 48'},
                labelElement: {xywh: '0 24 50 24'},
                bodyElement: {xywh: '0 0 24 24'},
                beforeInputElement: {xywh: '1 1 0 22'},
                inputElement: {xywh: '1 1 22 22'},
                afterInputElement: {xywh: '23 1 0 22'},
                errorElement: {xywh: '24 4 26 16'},
                errorIconElement: {xywh: '29 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under',
                triggers: null
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
                errorTarget: 'under',
                triggers: null
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
                errorTarget: 'under',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 0 50 24'},
                bodyElement: {xywh: '0 24 50 24'},
                beforeInputElement: {xywh: '1 25 0 22'},
                inputElement: {xywh: '1 25 48 22'},
                afterInputElement: {xywh: '49 25 0 22'},
                errorElement: {xywh: '0 50 50 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 29 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under',
                triggers: null
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 50 68'},
                labelElement: {xywh: '0 44 50 24'},
                bodyElement: {xywh: '0 0 50 24'},
                beforeInputElement: {xywh: '1 1 0 22'},
                inputElement: {xywh: '1 1 48 22'},
                afterInputElement: {xywh: '49 1 0 22'},
                errorElement: {xywh: '0 26 50 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 29 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('stretched height', function () {
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
                element: {xywh: '0 0 270 100'},
                bodyElement: {xywh: '100 0 144 100'},
                labelElement: {xywh: '0 0 100 100'},
                inputElement: {xywh: '145 1 54 98'},
                beforeInputElement: {xywh: '101 1 44 98'},
                afterInputElement: {xywh: '199 1 44 98'},
                errorElement: {xywh: '244 42 26 16'},
                errorIconElement: {xywh: '249 42 16 16'},
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
                element: {xywh: '0 0 270 100'},
                bodyElement: {xywh: '26 0 144 100'},
                labelElement: {xywh: '170 0 100 100'},
                inputElement: {xywh: '71 1 54 98'},
                beforeInputElement: {xywh: '27 1 44 98'},
                afterInputElement: {xywh: '125 1 44 98'},
                errorElement: {xywh: '0 42 26 16'},
                errorIconElement: {xywh: '5 42 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 100'},
                bodyElement: {xywh: '0 24 144 76'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 54 74'},
                beforeInputElement: {xywh: '1 25 44 74'},
                afterInputElement: {xywh: '99 25 44 74'},
                errorElement: {xywh: '144 54 26 16'},
                errorIconElement: {xywh: '149 54 16 16'},
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
                element: {xywh: '0 0 170 100'},
                bodyElement: {xywh: '0 0 144 76'},
                labelElement: {xywh: '0 76 170 24'},
                inputElement: {xywh: '45 1 54 74'},
                beforeInputElement: {xywh: '1 1 44 74'},
                afterInputElement: {xywh: '99 1 44 74'},
                errorElement: {xywh: '144 30 26 16'},
                errorIconElement: {xywh: '149 30 16 16'},
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
                element: {xywh: '0 0 270 100'},
                bodyElement: {xywh: '100 0 170 80'},
                labelElement: {xywh: '0 0 100 100'},
                inputElement: {xywh: '145 1 80 78'},
                beforeInputElement: {xywh: '101 1 44 78'},
                afterInputElement: {xywh: '225 1 44 78'},
                errorElement: {xywh: '100 82 170 16'},
                errorIconElement: {xywh: '100 82 16 16'},
                errorMessageElement: {xywh: '121 82 149 16'},
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
                element: {xywh: '0 0 270 100'},
                bodyElement: {xywh: '0 0 170 80'},
                labelElement: {xywh: '170 0 100 100'},
                inputElement: {xywh: '45 1 80 78'},
                beforeInputElement: {xywh: '1 1 44 78'},
                afterInputElement: {xywh: '125 1 44 78'},
                errorElement: {xywh: '0 82 170 16'},
                errorIconElement: {xywh: '0 82 16 16'},
                errorMessageElement: {xywh: '21 82 149 16'},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 100'},
                bodyElement: {xywh: '0 24 170 56'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 80 54'},
                beforeInputElement: {xywh: '1 25 44 54'},
                afterInputElement: {xywh: '125 25 44 54'},
                errorElement: {xywh: '0 82 170 16'},
                errorIconElement: {xywh: '0 82 16 16'},
                errorMessageElement: {xywh: '21 82 149 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 100'},
                bodyElement: {xywh: '0 0 170 56'},
                labelElement: {xywh: '0 76 170 24'},
                inputElement: {xywh: '45 1 80 54'},
                beforeInputElement: {xywh: '1 1 44 54'},
                afterInputElement: {xywh: '125 1 44 54'},
                errorElement: {xywh: '0 58 170 16'},
                errorIconElement: {xywh: '0 58 16 16'},
                errorMessageElement: {xywh: '21 58 149 16'},
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
                element: {xywh: '0 0 270 24'},
                bodyElement: {xywh: '100 0 144 24'},
                labelElement: {xywh: '0 0 100 24'},
                inputElement: {xywh: '145 1 54 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '199 1 44 22'},
                errorElement: {xywh: '244 4 26 16'},
                errorIconElement: {xywh: '249 4 16 16'},
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
                element: {xywh: '0 0 270 24'},
                bodyElement: {xywh: '26 0 144 24'},
                labelElement: {xywh: '170 0 100 24'},
                inputElement: {xywh: '71 1 54 22'},
                beforeInputElement: {xywh: '27 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 48'},
                bodyElement: {xywh: '0 24 144 24'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 54 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '99 25 44 22'},
                errorElement: {xywh: '144 28 26 16'},
                errorIconElement: {xywh: '149 28 16 16'},
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
                element: {xywh: '0 0 170 48'},
                bodyElement: {xywh: '0 0 144 24'},
                labelElement: {xywh: '0 24 170 24'},
                inputElement: {xywh: '45 1 54 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '99 1 44 22'},
                errorElement: {xywh: '144 4 26 16'},
                errorIconElement: {xywh: '149 4 16 16'},
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
                element: {xywh: '0 0 270 44'},
                bodyElement: {xywh: '100 0 170 24'},
                labelElement: {xywh: '0 0 100 44'},
                inputElement: {xywh: '145 1 80 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '225 1 44 22'},
                errorElement: {xywh: '100 26 170 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 149 16'},
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
                element: {xywh: '0 0 270 44'},
                bodyElement: {xywh: '0 0 170 24'},
                labelElement: {xywh: '170 0 100 44'},
                inputElement: {xywh: '45 1 80 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 26 170 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 149 16'},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 68'},
                bodyElement: {xywh: '0 24 170 24'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 80 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '125 25 44 22'},
                errorElement: {xywh: '0 50 170 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 149 16'},
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
                element: {xywh: '0 0 170 68'},
                bodyElement: {xywh: '0 0 170 24'},
                labelElement: {xywh: '0 44 170 24'},
                inputElement: {xywh: '45 1 80 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 26 170 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 149 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('inline', function () {
        it('should layout with left label and side error', function () {
            create({
                inline: true,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 270 24'},
                bodyElement: {xywh: '100 0 144 24'},
                labelElement: {xywh: '0 0 100 24'},
                inputElement: {xywh: '145 1 54 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '199 1 44 22'},
                errorElement: {xywh: '244 4 26 16'},
                errorIconElement: {xywh: '249 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
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
                element: {xywh: '0 0 270 24'},
                bodyElement: {xywh: '26 0 144 24'},
                labelElement: {xywh: '170 0 100 24'},
                inputElement: {xywh: '71 1 54 22'},
                beforeInputElement: {xywh: '27 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '176 4 50 17'}
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
                element: {xywh: '0 0 170 48'},
                bodyElement: {xywh: '0 24 144 24'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 54 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '99 25 44 22'},
                errorElement: {xywh: '144 28 26 16'},
                errorIconElement: {xywh: '149 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
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
                element: {xywh: '0 0 170 48'},
                bodyElement: {xywh: '0 0 144 24'},
                labelElement: {xywh: '0 24 170 24'},
                inputElement: {xywh: '45 1 54 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '99 1 44 22'},
                errorElement: {xywh: '144 4 26 16'},
                errorIconElement: {xywh: '149 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
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
                element: {xywh: '0 0 270 44'},
                bodyElement: {xywh: '100 0 170 24'},
                labelElement: {xywh: '0 0 100 44'},
                inputElement: {xywh: '145 1 80 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '225 1 44 22'},
                errorElement: {xywh: '100 26 170 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 149 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
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
                element: {xywh: '0 0 270 44'},
                bodyElement: {xywh: '0 0 170 24'},
                labelElement: {xywh: '170 0 100 44'},
                inputElement: {xywh: '45 1 80 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 26 170 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 149 16'},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with top label and under error', function () {
            create({
                inline: true,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 68'},
                bodyElement: {xywh: '0 24 170 24'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 80 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '125 25 44 22'},
                errorElement: {xywh: '0 50 170 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 149 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with bottom label and under error', function () {
            create({
                inline: true,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 68'},
                bodyElement: {xywh: '0 0 170 24'},
                labelElement: {xywh: '0 44 170 24'},
                inputElement: {xywh: '45 1 80 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 26 170 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 149 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('floated', function () {
        it('should layout with left label and side error', function () {
            create({
                floated: true,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 270 24'},
                bodyElement: {xywh: '100 0 144 24'},
                labelElement: {xywh: '0 0 100 24'},
                inputElement: {xywh: '145 1 54 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '199 1 44 22'},
                errorElement: {xywh: '244 4 26 16'},
                errorIconElement: {xywh: '249 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                floated: true,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 270 24'},
                bodyElement: {xywh: '26 0 144 24'},
                labelElement: {xywh: '170 0 100 24'},
                inputElement: {xywh: '71 1 54 22'},
                beforeInputElement: {xywh: '27 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 4 26 16'},
                errorIconElement: {xywh: '5 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                floated: true,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 48'},
                bodyElement: {xywh: '0 24 144 24'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 54 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '99 25 44 22'},
                errorElement: {xywh: '144 28 26 16'},
                errorIconElement: {xywh: '149 28 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                floated: true,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 48'},
                bodyElement: {xywh: '0 0 144 24'},
                labelElement: {xywh: '0 24 170 24'},
                inputElement: {xywh: '45 1 54 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '99 1 44 22'},
                errorElement: {xywh: '144 4 26 16'},
                errorIconElement: {xywh: '149 4 16 16'},
                errorMessageElement: {d: false},
                labelHtmlElement: {xywh: '0 28 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                floated: true,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 270 44'},
                bodyElement: {xywh: '100 0 170 24'},
                labelElement: {xywh: '0 0 100 44'},
                inputElement: {xywh: '145 1 80 22'},
                beforeInputElement: {xywh: '101 1 44 22'},
                afterInputElement: {xywh: '225 1 44 22'},
                errorElement: {xywh: '100 26 170 16'},
                errorIconElement: {xywh: '100 26 16 16'},
                errorMessageElement: {xywh: '121 26 149 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                floated: true,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 270 44'},
                bodyElement: {xywh: '0 0 170 24'},
                labelElement: {xywh: '170 0 100 44'},
                inputElement: {xywh: '45 1 80 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 26 170 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 149 16'},
                labelHtmlElement: {xywh: '176 4 50 17'}
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with top label and under error', function () {
            create({
                floated: true,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 68'},
                bodyElement: {xywh: '0 24 170 24'},
                labelElement: {xywh: '0 0 170 24'},
                inputElement: {xywh: '45 25 80 22'},
                beforeInputElement: {xywh: '1 25 44 22'},
                afterInputElement: {xywh: '125 25 44 22'},
                errorElement: {xywh: '0 50 170 16'},
                errorIconElement: {xywh: '0 50 16 16'},
                errorMessageElement: {xywh: '21 50 149 16'},
                labelHtmlElement: {xywh: '0 4 50 17'}
            });
        });

        (Ext.isIE11 ? xit : it)('should layout with bottom label and under error', function () {
            create({
                floated: true,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: {xywh: '0 0 170 68'},
                bodyElement: {xywh: '0 0 170 24'},
                labelElement: {xywh: '0 44 170 24'},
                inputElement: {xywh: '45 1 80 22'},
                beforeInputElement: {xywh: '1 1 44 22'},
                afterInputElement: {xywh: '125 1 44 22'},
                errorElement: {xywh: '0 26 170 16'},
                errorIconElement: {xywh: '0 26 16 16'},
                errorMessageElement: {xywh: '21 26 149 16'},
                labelHtmlElement: {xywh: '0 48 50 17'}
            });
        });
    });

    describe('fit layout', function () {
        it('should grow', function () {
            create({
                label: null
            }, {
                layout: 'fit',
                height: 100,
                width: 300
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 100' },
                bodyElement: { xywh: '0 0 300 100' },
                labelElement: { d: false },
                inputElement: { xywh: '45 1 210 98' },
                beforeInputElement: { xywh: '1 1 44 98' },
                afterInputElement: { xywh: '255 1 44 98' },
                errorElement: { d: false },
                errorIconElement: { d: false },
                errorMessageElement: { d: false }
            });
        });

        it('should shrink', function () {
            create({
                label: null
            }, {
                layout: 'fit',
                width: 120
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 120 24' },
                labelElement: { d: false },
                bodyElement: { xywh: '0 0 120 24' },
                beforeInputElement: { xywh: '1 1 44 22' },
                inputElement: { xywh: '45 1 30 22' },
                afterInputElement: { xywh: '75 1 44 22' },
                errorElement: { d: false },
                errorIconElement: { d: false },
                errorMessageElement: { d: false }
            });
        });
    });
});























































