describe('Ext.field.TextArea_layout', function () {
    var labelHtml = '<span style="display:inline-block;width:50px;background:green;">&nbsp;</span>',
        pctBug = Ext.supports.PercentageSizeFlexBug,
        container, field;

    function create(config, containerConfig) {
        field = new Ext.field.TextArea(Ext.apply({
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
                element: { xywh: '0 0 600 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 474 75' },
                inputElement: { xywh: '101 1 472 73' },
                errorElement: { xywh: '574 30 26 16' },
                errorIconElement: { xywh: '579 30 16 16' },
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
                element: { xywh: '0 0 600 75' },
                labelElement: { xywh: '500 0 100 75' },
                bodyElement: { xywh: '26 0 474 75' },
                inputElement: { xywh: '27 1 472 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
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
                element: { xywh: '0 0 600 99' },
                labelElement: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '0 24 574 75' },
                inputElement: { xywh: '1 25 572 73' },
                errorElement: { xywh: '574 54 26 16' },
                errorIconElement: { xywh: '579 54 16 16' },
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
                element: { xywh: '0 0 600 99' },
                labelElement: { xywh: '0 75 600 24' },
                bodyElement: { xywh: '0 0 574 75' },
                inputElement: { xywh: '1 1 572 73' },
                errorElement: { xywh: '574 30 26 16' },
                errorIconElement: { xywh: '579 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 79 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 500 55' },
                inputElement: { xywh: '101 1 498 53' },
                errorElement: { xywh: '100 57 500 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 479 16' },
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
                element: { xywh: '0 0 600 75' },
                labelElement: { xywh: '500 0 100 75' },
                bodyElement: { xywh: '0 0 500 55' },
                inputElement: { xywh: '1 1 498 53' },
                errorElement: { xywh: '0 57 500 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 479 16' },
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
                element: { xywh: '0 0 600 99' },
                labelElement: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '0 24 600 55' },
                inputElement: { xywh: '1 25 598 53' },
                errorElement: { xywh: '0 81 600 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 579 16' },
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
                element: { xywh: '0 0 600 99' },
                labelElement: { xywh: '0 75 600 24' },
                bodyElement: { xywh: '0 0 600 55' },
                inputElement: { xywh: '1 1 598 53' },
                errorElement: { xywh: '0 57 600 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 579 16' },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 274 75' },
                inputElement: { xywh: '101 1 272 73' },
                errorElement: { xywh: '374 30 26 16' },
                errorIconElement: { xywh: '379 30 16 16' },
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
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '300 0 100 75' },
                bodyElement: { xywh: '26 0 274 75' },
                inputElement: { xywh: '27 1 272 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
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
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 374 75' },
                inputElement: { xywh: '1 25 372 73' },
                errorElement: { xywh: '374 54 26 16' },
                errorIconElement: { xywh: '379 54 16 16' },
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
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 75 400 24' },
                bodyElement: { xywh: '0 0 374 75' },
                inputElement: { xywh: '1 1 372 73' },
                errorElement: { xywh: '374 30 26 16' },
                errorIconElement: { xywh: '379 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 300 55' },
                inputElement: { xywh: '101 1 298 53' },
                errorElement: { xywh: '100 57 300 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 279 16' },
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
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '300 0 100 75' },
                bodyElement: { xywh: '0 0 300 55' },
                inputElement: { xywh: '1 1 298 53' },
                errorElement: { xywh: '0 57 300 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 279 16' },
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
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 400 55' },
                inputElement: { xywh: '1 25 398 53' },
                errorElement: { xywh: '0 81 400 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 379 16' },
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
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 75 400 24' },
                bodyElement: { xywh: '0 0 400 55' },
                inputElement: { xywh: '1 1 398 53' },
                errorElement: { xywh: '0 57 400 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 379 16' },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 114 75' },
                inputElement: { xywh: '101 1 112 73' },
                errorElement: { xywh: '214 30 26 16' },
                errorIconElement: { xywh: '219 30 16 16' },
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
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '140 0 100 75' },
                bodyElement: { xywh: '26 0 114 75' },
                inputElement: { xywh: '27 1 112 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 114 75' },
                inputElement: { xywh: '1 25 112 73' },
                errorElement: { xywh: '114 54 26 16' },
                errorIconElement: { xywh: '119 54 16 16' },
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 75 140 24' },
                bodyElement: { xywh: '0 0 114 75' },
                inputElement: { xywh: '1 1 112 73' },
                errorElement: { xywh: '114 30 26 16' },
                errorIconElement: { xywh: '119 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 140 55' },
                inputElement: { xywh: '101 1 138 53' },
                errorElement: { xywh: '100 57 140 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 119 16' },
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
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '140 0 100 75' },
                bodyElement: { xywh: '0 0 140 55' },
                inputElement: { xywh: '1 1 138 53' },
                errorElement: { xywh: '0 57 140 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 119 16' },
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 140 55' },
                inputElement: { xywh: '1 25 138 53' },
                errorElement: { xywh: '0 81 140 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 119 16' },
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 75 140 24' },
                bodyElement: { xywh: '0 0 140 55' },
                inputElement: { xywh: '1 1 138 53' },
                errorElement: { xywh: '0 57 140 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 119 16' },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '50 0 0 75' }
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
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '0 0 0 75' }
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
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 24 75' },
                inputElement: { xywh: '1 25 22 73' },
                errorElement: { xywh: '24 54 26 16' },
                errorIconElement: { xywh: '29 54 16 16' },
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
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 75 50 24' },
                bodyElement: { xywh: '0 0 24 75' },
                inputElement: { xywh: '1 1 22 73' },
                errorElement: { xywh: '24 30 26 16' },
                errorIconElement: { xywh: '29 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '50 0 0 75'}
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
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '0 0 0 75'}
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
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 50 55' },
                inputElement: { xywh: '1 25 48 53' },
                errorElement: { xywh: '0 81 50 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 29 16' },
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
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 75 50 24' },
                bodyElement: { xywh: '0 0 50 55' },
                inputElement: { xywh: '1 1 48 53' },
                errorElement: { xywh: '0 57 50 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 29 16' },
                labelHtmlElement: { xywh: '0 79 50 17' }
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

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 400 41' },
                labelElement: { xywh: '0 0 100 41' },
                bodyElement: { xywh: '100 0 274 41' },
                inputElement: { xywh: '101 1 272 39' },
                errorElement: { xywh: '374 13 26 16' },
                errorIconElement: { xywh: '379 13 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            } : {
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 274 75' },
                inputElement: { xywh: '101 1 272 73' },
                errorElement: { xywh: '374 30 26 16' },
                errorIconElement: { xywh: '379 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 400 41' },
                labelElement: { xywh: '300 0 100 41' },
                bodyElement: { xywh: '26 0 274 41' },
                inputElement: { xywh: '27 1 272 39' },
                errorElement: { xywh: '0 13 26 16' },
                errorIconElement: { xywh: '5 13 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '306 4 50 17' }
            } : {
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '300 0 100 75' },
                bodyElement: { xywh: '26 0 274 75' },
                inputElement: { xywh: '27 1 272 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 374 75' },
                inputElement: { xywh: '1 25 372 73' },
                errorElement: { xywh: '374 54 26 16' },
                errorIconElement: { xywh: '379 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 75 400 24' },
                bodyElement: { xywh: '0 0 374 75' },
                inputElement: { xywh: '1 1 372 73' },
                errorElement: { xywh: '374 30 26 16' },
                errorIconElement: { xywh: '379 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 400 61' },
                labelElement: { xywh: '0 0 100 61' },
                bodyElement: { xywh: '100 0 300 41' },
                inputElement: { xywh: '101 1 298 39' },
                errorElement: { xywh: '100 43 300 16' },
                errorIconElement: { xywh: '100 43 16 16' },
                errorMessageElement: { xywh: '121 43 279 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            } : {
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 300 55' },
                inputElement: { xywh: '101 1 298 53' },
                errorElement: { xywh: '100 57 300 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 279 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 400 61' },
                labelElement: { xywh: '300 0 100 61' },
                bodyElement: { xywh: '0 0 300 41' },
                inputElement: { xywh: '1 1 298 39' },
                errorElement: { xywh: '0 43 300 16' },
                errorIconElement: { xywh: '0 43 16 16' },
                errorMessageElement: { xywh: '21 43 279 16' },
                labelHtmlElement: { xywh: '306 4 50 17' }
            } : {
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '300 0 100 75' },
                bodyElement: { xywh: '0 0 300 55' },
                inputElement: { xywh: '1 1 298 53' },
                errorElement: { xywh: '0 57 300 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 279 16' },
                labelHtmlElement: { xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 400 55' },
                inputElement: { xywh: '1 25 398 53' },
                errorElement: { xywh: '0 81 400 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 379 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 75 400 24' },
                bodyElement: { xywh: '0 0 400 55' },
                inputElement: { xywh: '1 1 398 53' },
                errorElement: { xywh: '0 57 400 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 379 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
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

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 240 41' },
                labelElement: { xywh: '0 0 100 41' },
                bodyElement: { xywh: '100 0 114 41' },
                inputElement: { xywh: '101 1 112 39' },
                errorElement: { xywh: '214 13 26 16' },
                errorIconElement: { xywh: '219 13 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            } : {
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 114 75' },
                inputElement: { xywh: '101 1 112 73' },
                errorElement: { xywh: '214 30 26 16' },
                errorIconElement: { xywh: '219 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 240 41' },
                labelElement: { xywh: '140 0 100 41' },
                bodyElement: { xywh: '26 0 114 41' },
                inputElement: { xywh: '27 1 112 39' },
                errorElement: { xywh: '0 13 26 16' },
                errorIconElement: { xywh: '5 13 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '146 4 50 17' }
            } : {
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '140 0 100 75' },
                bodyElement: { xywh: '26 0 114 75' },
                inputElement: { xywh: '27 1 112 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '146 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 114 75' },
                inputElement: { xywh: '1 25 112 73' },
                errorElement: { xywh: '114 54 26 16' },
                errorIconElement: { xywh: '119 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 75 140 24' },
                bodyElement: { xywh: '0 0 114 75' },
                inputElement: { xywh: '1 1 112 73' },
                errorElement: { xywh: '114 30 26 16' },
                errorIconElement: { xywh: '119 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 240 61' },
                labelElement: { xywh: '0 0 100 61' },
                bodyElement: { xywh: '100 0 140 41' },
                inputElement: { xywh: '101 1 138 39' },
                errorElement: { xywh: '100 43 140 16' },
                errorIconElement: { xywh: '100 43 16 16' },
                errorMessageElement: { xywh: '121 43 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            } : {
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 140 55' },
                inputElement: { xywh: '101 1 138 53' },
                errorElement: { xywh: '100 57 140 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 240 61' },
                labelElement: { xywh: '140 0 100 61' },
                bodyElement: { xywh: '0 0 140 41' },
                inputElement: { xywh: '1 1 138 39' },
                errorElement: { xywh: '0 43 140 16' },
                errorIconElement: { xywh: '0 43 16 16' },
                errorMessageElement: { xywh: '21 43 119 16' },
                labelHtmlElement: { xywh: '146 4 50 17' }
            } : {
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '140 0 100 75' },
                bodyElement: { xywh: '0 0 140 55' },
                inputElement: { xywh: '1 1 138 53' },
                errorElement: { xywh: '0 57 140 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 119 16' },
                labelHtmlElement: { xywh: '146 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 140 55' },
                inputElement: { xywh: '1 25 138 53' },
                errorElement: { xywh: '0 81 140 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 75 140 24' },
                bodyElement: { xywh: '0 0 140 55' },
                inputElement: { xywh: '1 1 138 53' },
                errorElement: { xywh: '0 57 140 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 119 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
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

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 50 41' },
                labelElement: { xywh: '0 0 50 41' },
                bodyWrapElement: { xywh: '50 0 0 41'}
            } : {
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '50 0 0 75'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 50 41' },
                labelElement: { xywh: '0 0 50 41' },
                bodyWrapElement: { xywh: '0 0 0 41'}
            } : {
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '0 0 0 75'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 24 75' },
                inputElement: { xywh: '1 25 22 73' },
                errorElement: { xywh: '24 54 26 16' },
                errorIconElement: { xywh: '29 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 75 50 24' },
                bodyElement: { xywh: '0 0 24 75' },
                inputElement: { xywh: '1 1 22 73' },
                errorElement: { xywh: '24 30 26 16' },
                errorIconElement: { xywh: '29 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 50 61' },
                labelElement: { xywh: '0 0 50 61' },
                bodyWrapElement: { xywh: '50 0 0 61'}
            } : {
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '50 0 0 75'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout(pctBug ? {
                element: { xywh: '0 0 50 61' },
                labelElement: { xywh: '0 0 50 61' },
                bodyWrapElement: { xywh: '0 0 0 61'}
            } : {
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '0 0 0 75'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 50 55' },
                inputElement: { xywh: '1 25 48 53' },
                errorElement: { xywh: '0 81 50 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 29 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 75 50 24' },
                bodyElement: { xywh: '0 0 50 55' },
                inputElement: { xywh: '1 1 48 53' },
                errorElement: { xywh: '0 57 50 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 29 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });
    });

    describe('configured height - larger than default height', function () {
        it('should layout with left label and side error', function () {
            create({
                height: 150,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '0 0 100 150' },
                bodyElement: { xywh: '100 0 474 150' },
                inputElement: { xywh: '101 1 472 148' },
                errorElement: { xywh: '574 67 26 16' },
                errorIconElement: { xywh: '579 67 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                height: 150,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '500 0 100 150' },
                bodyElement: { xywh: '26 0 474 150' },
                inputElement: { xywh: '27 1 472 148' },
                errorElement: { xywh: '0 67 26 16' },
                errorIconElement: { xywh: '5 67 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                height: 150,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '0 24 574 126' },
                inputElement: { xywh: '1 25 572 124' },
                errorElement: { xywh: '574 79 26 16' },
                errorIconElement: { xywh: '579 79 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                height: 150,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '0 126 600 24' },
                bodyElement: { xywh: '0 0 574 126' },
                inputElement: { xywh: '1 1 572 124' },
                errorElement: { xywh: '574 55 26 16' },
                errorIconElement: { xywh: '579 55 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 130 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                height: 150,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '0 0 100 150' },
                bodyElement: { xywh: '100 0 500 130' },
                inputElement: { xywh: '101 1 498 128' },
                errorElement: { xywh: '100 132 500 16' },
                errorIconElement: { xywh: '100 132 16 16' },
                errorMessageElement: { xywh: '121 132 479 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                height: 150,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '500 0 100 150' },
                bodyElement: { xywh: '0 0 500 130' },
                inputElement: { xywh: '1 1 498 128' },
                errorElement: { xywh: '0 132 500 16' },
                errorIconElement: { xywh: '0 132 16 16' },
                errorMessageElement: { xywh: '21 132 479 16' },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                height: 150,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '0 24 600 106' },
                inputElement: { xywh: '1 25 598 104' },
                errorElement: { xywh: '0 132 600 16' },
                errorIconElement: { xywh: '0 132 16 16' },
                errorMessageElement: { xywh: '21 132 579 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                height: 150,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 150' },
                labelElement: { xywh: '0 126 600 24' },
                bodyElement: { xywh: '0 0 600 106' },
                inputElement: { xywh: '1 1 598 104' },
                errorElement: { xywh: '0 108 600 16' },
                errorIconElement: { xywh: '0 108 16 16' },
                errorMessageElement: { xywh: '21 108 579 16' },
                labelHtmlElement: { xywh: '0 130 50 17' }
            });
        });
    });

    describe('configured height - smaller than default height', function () {
        it('should layout with left label and side error', function () {
            create({
                height: 60,
                labelAlign: 'left',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '0 0 100 60' },
                bodyElement: { xywh: '100 0 474 60' },
                inputElement: { xywh: '101 1 472 58' },
                errorElement: { xywh: '574 22 26 16' },
                errorIconElement: { xywh: '579 22 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and side error', function () {
            create({
                height: 60,
                labelAlign: 'right',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '500 0 100 60' },
                bodyElement: { xywh: '26 0 474 60' },
                inputElement: { xywh: '27 1 472 58' },
                errorElement: { xywh: '0 22 26 16' },
                errorIconElement: { xywh: '5 22 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and side error', function () {
            create({
                height: 60,
                labelAlign: 'top',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '0 24 574 36' },
                inputElement: { xywh: '1 25 572 34' },
                errorElement: { xywh: '574 34 26 16' },
                errorIconElement: { xywh: '579 34 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                height: 60,
                labelAlign: 'bottom',
                errorTarget: 'side'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '0 36 600 24' },
                bodyElement: { xywh: '0 0 574 36' },
                inputElement: { xywh: '1 1 572 34' },
                errorElement: { xywh: '574 10 26 16' },
                errorIconElement: { xywh: '579 10 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 40 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                height: 60,
                labelAlign: 'left',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '0 0 100 60' },
                bodyElement: { xywh: '100 0 500 40' },
                inputElement: { xywh: '101 1 498 38' },
                errorElement: { xywh: '100 42 500 16' },
                errorIconElement: { xywh: '100 42 16 16' },
                errorMessageElement: { xywh: '121 42 479 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with right label and under error', function () {
            create({
                height: 60,
                labelAlign: 'right',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '500 0 100 60' },
                bodyElement: { xywh: '0 0 500 40' },
                inputElement: { xywh: '1 1 498 38' },
                errorElement: { xywh: '0 42 500 16' },
                errorIconElement: { xywh: '0 42 16 16' },
                errorMessageElement: { xywh: '21 42 479 16' },
                labelHtmlElement: { xywh: '506 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                height: 60,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '0 0 600 24' },
                bodyElement: { xywh: '0 24 600 16' },
                inputElement: { xywh: '1 25 598 22' },
                errorElement: { xywh: '0 42 600 16' },
                errorIconElement: { xywh: '0 42 16 16' },
                errorMessageElement: { xywh: '21 42 579 16' },
                labelHtmlElement: { xywh: '0 4 50 17' }
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                height: 60,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 600 60' },
                labelElement: { xywh: '0 36 600 24' },
                bodyElement: { xywh: '0 0 600 16' },
                inputElement: { xywh: '1 1 598 22' },
                errorElement: { xywh: '0 18 600 16' },
                errorIconElement: { xywh: '0 18 16 16' },
                errorMessageElement: { xywh: '21 18 579 16' },
                labelHtmlElement: { xywh: '0 40 50 17' }
            });
        });
    });

    describe('flex grow height', function () {
        var containerConfig = {
            height: 150,
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
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '0 0 100 150' },
                bodyElement: { xywh: '100 0 144 150' },
                inputElement: { xywh: '101 1 142 148' },
                errorElement: { xywh: '244 67 26 16' },
                errorIconElement: { xywh: '249 67 16 16' },
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
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '170 0 100 150' },
                bodyElement: { xywh: '26 0 144 150' },
                inputElement: { xywh: '27 1 142 148' },
                errorElement: { xywh: '0 67 26 16' },
                errorIconElement: { xywh: '5 67 16 16' },
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
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 144 126' },
                inputElement: { xywh: '1 25 142 124' },
                errorElement: { xywh: '144 79 26 16' },
                errorIconElement: { xywh: '149 79 16 16' },
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
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 126 170 24' },
                bodyElement: { xywh: '0 0 144 126' },
                inputElement: { xywh: '1 1 142 124' },
                errorElement: { xywh: '144 55 26 16' },
                errorIconElement: { xywh: '149 55 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 130 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '0 0 100 150' },
                bodyElement: { xywh: '100 0 170 130' },
                inputElement: { xywh: '101 1 168 128' },
                errorElement: { xywh: '100 132 170 16' },
                errorIconElement: { xywh: '100 132 16 16' },
                errorMessageElement: { xywh: '121 132 149 16' },
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
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '170 0 100 150' },
                bodyElement: { xywh: '0 0 170 130' },
                inputElement: { xywh: '1 1 168 128' },
                errorElement: { xywh: '0 132 170 16' },
                errorIconElement: { xywh: '0 132 16 16' },
                errorMessageElement: { xywh: '21 132 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 170 106' },
                inputElement: { xywh: '1 25 168 104' },
                errorElement: { xywh: '0 132 170 16' },
                errorIconElement: { xywh: '0 132 16 16' },
                errorMessageElement: { xywh: '21 132 149 16' },
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
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 126 170 24' },
                bodyElement: { xywh: '0 0 170 106' },
                inputElement: { xywh: '1 1 168 104' },
                errorElement: { xywh: '0 108 170 16' },
                errorIconElement: { xywh: '0 108 16 16' },
                errorMessageElement: { xywh: '21 108 149 16' },
                labelHtmlElement: { xywh: '0 130 50 17' }
            });
        });
    });

    describe('flex shrink height', function () {
        var containerConfig = {
            height: 60,
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
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '0 0 100 60' },
                bodyElement: { xywh: '100 0 144 60' },
                inputElement: { xywh: '101 1 142 58' },
                errorElement: { xywh: '244 22 26 16' },
                errorIconElement: { xywh: '249 22 16 16' },
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
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '170 0 100 60' },
                bodyElement: { xywh: '26 0 144 60' },
                inputElement: { xywh: '27 1 142 58' },
                errorElement: { xywh: '0 22 26 16' },
                errorIconElement: { xywh: '5 22 16 16' },
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
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 144 36' },
                inputElement: { xywh: '1 25 142 34' },
                errorElement: { xywh: '144 34 26 16' },
                errorIconElement: { xywh: '149 34 16 16' },
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
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 36 170 24' },
                bodyElement: { xywh: '0 0 144 36' },
                inputElement: { xywh: '1 1 142 34' },
                errorElement: { xywh: '144 10 26 16' },
                errorIconElement: { xywh: '149 10 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 40 50 17' }
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '0 0 100 60' },
                bodyElement: { xywh: '100 0 170 40' },
                inputElement: { xywh: '101 1 168 38' },
                errorElement: { xywh: '100 42 170 16' },
                errorIconElement: { xywh: '100 42 16 16' },
                errorMessageElement: { xywh: '121 42 149 16' },
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
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '170 0 100 60' },
                bodyElement: { xywh: '0 0 170 40' },
                inputElement: { xywh: '1 1 168 38' },
                errorElement: { xywh: '0 42 170 16' },
                errorIconElement: { xywh: '0 42 16 16' },
                errorMessageElement: { xywh: '21 42 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17' }
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 170 16' },
                inputElement: { xywh: '1 25 168 22' },
                errorElement: { xywh: '0 42 170 16' },
                errorIconElement: { xywh: '0 42 16 16' },
                errorMessageElement: { xywh: '21 42 149 16' },
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
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 36 170 24' },
                bodyElement: { xywh: '0 0 170 16' },
                inputElement: { xywh: '1 1 168 22' },
                errorElement: { xywh: '0 18 170 16' },
                errorIconElement: { xywh: '0 18 16 16' },
                errorMessageElement: { xywh: '21 18 149 16' },
                labelHtmlElement: { xywh: '0 40 50 17' }
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
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 274 75' },
                inputElement: { xywh: '101 1 272 73' },
                errorElement: { xywh: '374 30 26 16' },
                errorIconElement: { xywh: '379 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '300 0 100 75' },
                bodyElement: { xywh: '26 0 274 75' },
                inputElement: { xywh: '27 1 272 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 374 75' },
                inputElement: { xywh: '1 25 372 73' },
                errorElement: { xywh: '374 54 26 16' },
                errorIconElement: { xywh: '379 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 75 400 24' },
                bodyElement: { xywh: '0 0 374 75' },
                inputElement: { xywh: '1 1 372 73' },
                errorElement: { xywh: '374 30 26 16' },
                errorIconElement: { xywh: '379 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 300 55' },
                inputElement: { xywh: '101 1 298 53' },
                errorElement: { xywh: '100 57 300 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 279 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 75' },
                labelElement: { xywh: '300 0 100 75' },
                bodyElement: { xywh: '0 0 300 55' },
                inputElement: { xywh: '1 1 298 53' },
                errorElement: { xywh: '0 57 300 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 279 16' },
                labelHtmlElement: { xywh: '306 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 0 400 24' },
                bodyElement: { xywh: '0 24 400 55' },
                inputElement: { xywh: '1 25 398 53' },
                errorElement: { xywh: '0 81 400 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 379 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 400 99' },
                labelElement: { xywh: '0 75 400 24' },
                bodyElement: { xywh: '0 0 400 55' },
                inputElement: { xywh: '1 1 398 53' },
                errorElement: { xywh: '0 57 400 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 379 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
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
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 114 75' },
                inputElement: { xywh: '101 1 112 73' },
                errorElement: { xywh: '214 30 26 16' },
                errorIconElement: { xywh: '219 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '140 0 100 75' },
                bodyElement: { xywh: '26 0 114 75' },
                inputElement: { xywh: '27 1 112 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '146 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 114 75' },
                inputElement: { xywh: '1 25 112 73' },
                errorElement: { xywh: '114 54 26 16' },
                errorIconElement: { xywh: '119 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 75 140 24' },
                bodyElement: { xywh: '0 0 114 75' },
                inputElement: { xywh: '1 1 112 73' },
                errorElement: { xywh: '114 30 26 16' },
                errorIconElement: { xywh: '119 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 140 55' },
                inputElement: { xywh: '101 1 138 53' },
                errorElement: { xywh: '100 57 140 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 240 75' },
                labelElement: { xywh: '140 0 100 75' },
                bodyElement: { xywh: '0 0 140 55' },
                inputElement: { xywh: '1 1 138 53' },
                errorElement: { xywh: '0 57 140 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 119 16' },
                labelHtmlElement: { xywh: '146 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 0 140 24' },
                bodyElement: { xywh: '0 24 140 55' },
                inputElement: { xywh: '1 25 138 53' },
                errorElement: { xywh: '0 81 140 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 119 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 140 99' },
                labelElement: { xywh: '0 75 140 24' },
                bodyElement: { xywh: '0 0 140 55' },
                inputElement: { xywh: '1 1 138 53' },
                errorElement: { xywh: '0 57 140 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 119 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
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
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '50 0 0 75'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '0 0 0 75'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 24 75' },
                inputElement: { xywh: '1 25 22 73' },
                errorElement: { xywh: '24 54 26 16' },
                errorIconElement: { xywh: '29 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 75 50 24' },
                bodyElement: { xywh: '0 0 24 75' },
                inputElement: { xywh: '1 1 22 73' },
                errorElement: { xywh: '24 30 26 16' },
                errorIconElement: { xywh: '29 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '50 0 0 75'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError('E');

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 75' },
                labelElement: { xywh: '0 0 50 75' },
                bodyWrapElement: { xywh: '0 0 0 75'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 0 50 24' },
                bodyElement: { xywh: '0 24 50 55' },
                inputElement: { xywh: '1 25 48 53' },
                errorElement: { xywh: '0 81 50 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 29 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 50 99' },
                labelElement: { xywh: '0 75 50 24' },
                bodyElement: { xywh: '0 0 50 55' },
                inputElement: { xywh: '1 1 48 53' },
                errorElement: { xywh: '0 57 50 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 29 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });
    });

    describe('stretched height - larger than default height', function () {
        var containerConfig = {
            height: 150,
            layout: 'hbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '0 0 100 150' },
                bodyElement: { xywh: '100 0 144 150' },
                inputElement: { xywh: '101 1 142 148' },
                errorElement: { xywh: '244 67 26 16' },
                errorIconElement: { xywh: '249 67 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '170 0 100 150' },
                bodyElement: { xywh: '26 0 144 150' },
                inputElement: { xywh: '27 1 142 148' },
                errorElement: { xywh: '0 67 26 16' },
                errorIconElement: { xywh: '5 67 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 144 126' },
                inputElement: { xywh: '1 25 142 124' },
                errorElement: { xywh: '144 79 26 16' },
                errorIconElement: { xywh: '149 79 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 126 170 24' },
                bodyElement: { xywh: '0 0 144 126' },
                inputElement: { xywh: '1 1 142 124' },
                errorElement: { xywh: '144 55 26 16' },
                errorIconElement: { xywh: '149 55 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 130 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '0 0 100 150' },
                bodyElement: { xywh: '100 0 170 130' },
                inputElement: { xywh: '101 1 168 128' },
                errorElement: { xywh: '100 132 170 16' },
                errorIconElement: { xywh: '100 132 16 16' },
                errorMessageElement: { xywh: '121 132 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 150' },
                labelElement: { xywh: '170 0 100 150' },
                bodyElement: { xywh: '0 0 170 130' },
                inputElement: { xywh: '1 1 168 128' },
                errorElement: { xywh: '0 132 170 16' },
                errorIconElement: { xywh: '0 132 16 16' },
                errorMessageElement: { xywh: '21 132 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 170 106' },
                inputElement: { xywh: '1 25 168 104' },
                errorElement: { xywh: '0 132 170 16' },
                errorIconElement: { xywh: '0 132 16 16' },
                errorMessageElement: { xywh: '21 132 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 150' },
                labelElement: { xywh: '0 126 170 24' },
                bodyElement: { xywh: '0 0 170 106' },
                inputElement: { xywh: '1 1 168 104' },
                errorElement: { xywh: '0 108 170 16' },
                errorIconElement: { xywh: '0 108 16 16' },
                errorMessageElement: { xywh: '21 108 149 16' },
                labelHtmlElement: { xywh: '0 130 50 17'}
            });
        });
    });

    describe('stretched height - smaller than default height', function () {
        var containerConfig = {
            height: 60,
            layout: 'hbox'
        };

        it('should layout with left label and side error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '0 0 100 60' },
                bodyElement: { xywh: '100 0 144 60' },
                inputElement: { xywh: '101 1 142 58' },
                errorElement: { xywh: '244 22 26 16' },
                errorIconElement: { xywh: '249 22 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and side error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '170 0 100 60' },
                bodyElement: { xywh: '26 0 144 60' },
                inputElement: { xywh: '27 1 142 58' },
                errorElement: { xywh: '0 22 26 16' },
                errorIconElement: { xywh: '5 22 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and side error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 144 36' },
                inputElement: { xywh: '1 25 142 34' },
                errorElement: { xywh: '144 34 26 16' },
                errorIconElement: { xywh: '149 34 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and side error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'side'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 36 170 24' },
                bodyElement: { xywh: '0 0 144 36' },
                inputElement: { xywh: '1 1 142 34' },
                errorElement: { xywh: '144 10 26 16' },
                errorIconElement: { xywh: '149 10 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 40 50 17'}
            });
        });

        it('should layout with left label and under error', function () {
            create({
                labelAlign: 'left',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '0 0 100 60' },
                bodyElement: { xywh: '100 0 170 40' },
                inputElement: { xywh: '101 1 168 38' },
                errorElement: { xywh: '100 42 170 16' },
                errorIconElement: { xywh: '100 42 16 16' },
                errorMessageElement: { xywh: '121 42 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with right label and under error', function () {
            create({
                labelAlign: 'right',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 270 60' },
                labelElement: { xywh: '170 0 100 60' },
                bodyElement: { xywh: '0 0 170 40' },
                inputElement: { xywh: '1 1 168 38' },
                errorElement: { xywh: '0 42 170 16' },
                errorIconElement: { xywh: '0 42 16 16' },
                errorMessageElement: { xywh: '21 42 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                labelAlign: 'top',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 170 16' },
                inputElement: { xywh: '1 25 168 22' },
                errorElement: { xywh: '0 42 170 16' },
                errorIconElement: { xywh: '0 42 16 16' },
                errorMessageElement: { xywh: '21 42 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                labelAlign: 'bottom',
                errorTarget: 'under'
            }, containerConfig);

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 60' },
                labelElement: { xywh: '0 36 170 24' },
                bodyElement: { xywh: '0 0 170 16' },
                inputElement: { xywh: '1 1 168 22' },
                errorElement: { xywh: '0 18 170 16' },
                errorIconElement: { xywh: '0 18 16 16' },
                errorMessageElement: { xywh: '21 18 149 16' },
                labelHtmlElement: { xywh: '0 40 50 17'}
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 144 75' },
                inputElement: { xywh: '101 1 142 73' },
                errorElement: { xywh: '244 30 26 16' },
                errorIconElement: { xywh: '249 30 16 16' },
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '170 0 100 75' },
                bodyElement: { xywh: '26 0 144 75' },
                inputElement: { xywh: '27 1 142 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '176 4 50 17' }
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
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 144 75' },
                inputElement: { xywh: '1 25 142 73' },
                errorElement: { xywh: '144 54 26 16' },
                errorIconElement: { xywh: '149 54 16 16' },
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
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 75 170 24' },
                bodyElement: { xywh: '0 0 144 75' },
                inputElement: { xywh: '1 1 142 73' },
                errorElement: { xywh: '144 30 26 16' },
                errorIconElement: { xywh: '149 30 16 16' },
                errorMessageElement: { d: false },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 170 55' },
                inputElement: { xywh: '101 1 168 53' },
                errorElement: { xywh: '100 57 170 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 149 16' },
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '170 0 100 75' },
                bodyElement: { xywh: '0 0 170 55' },
                inputElement: { xywh: '1 1 168 53' },
                errorElement: { xywh: '0 57 170 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17' }
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
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 170 55' },
                inputElement: { xywh: '1 25 168 53' },
                errorElement: { xywh: '0 81 170 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 149 16' },
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
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 75 170 24' },
                bodyElement: { xywh: '0 0 170 55' },
                inputElement: { xywh: '1 1 168 53' },
                errorElement: { xywh: '0 57 170 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 149 16' },
                labelHtmlElement: { xywh: '0 79 50 17' }
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 144 75' },
                inputElement: { xywh: '101 1 142 73' },
                errorElement: { xywh: '244 30 26 16' },
                errorIconElement: { xywh: '249 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '170 0 100 75' },
                bodyElement: { xywh: '26 0 144 75' },
                inputElement: { xywh: '27 1 142 73' },
                errorElement: { xywh: '0 30 26 16' },
                errorIconElement: { xywh: '5 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '176 4 50 17'}
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
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 144 75' },
                inputElement: { xywh: '1 25 142 73' },
                errorElement: { xywh: '144 54 26 16' },
                errorIconElement: { xywh: '149 54 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 75 170 24' },
                bodyElement: { xywh: '0 0 144 75' },
                inputElement: { xywh: '1 1 142 73' },
                errorElement: { xywh: '144 30 26 16' },
                errorIconElement: { xywh: '149 30 16 16' },
                errorMessageElement: {d: false},
                labelHtmlElement: { xywh: '0 79 50 17'}
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '0 0 100 75' },
                bodyElement: { xywh: '100 0 170 55' },
                inputElement: { xywh: '101 1 168 53' },
                errorElement: { xywh: '100 57 170 16' },
                errorIconElement: { xywh: '100 57 16 16' },
                errorMessageElement: { xywh: '121 57 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
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
                element: { xywh: '0 0 270 75' },
                labelElement: { xywh: '170 0 100 75' },
                bodyElement: { xywh: '0 0 170 55' },
                inputElement: { xywh: '1 1 168 53' },
                errorElement: { xywh: '0 57 170 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 149 16' },
                labelHtmlElement: { xywh: '176 4 50 17'}
            });
        });

        it('should layout with top label and under error', function () {
            create({
                floated: true,
                labelAlign: 'top',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 0 170 24' },
                bodyElement: { xywh: '0 24 170 55' },
                inputElement: { xywh: '1 25 168 53' },
                errorElement: { xywh: '0 81 170 16' },
                errorIconElement: { xywh: '0 81 16 16' },
                errorMessageElement: { xywh: '21 81 149 16' },
                labelHtmlElement: { xywh: '0 4 50 17'}
            });
        });

        it('should layout with bottom label and under error', function () {
            create({
                floated: true,
                labelAlign: 'bottom',
                errorTarget: 'under'
            });

            setError();

            expect(field).toHaveLayout({
                element: { xywh: '0 0 170 99' },
                labelElement: { xywh: '0 75 170 24' },
                bodyElement: { xywh: '0 0 170 55' },
                inputElement: { xywh: '1 1 168 53' },
                errorElement: { xywh: '0 57 170 16' },
                errorIconElement: { xywh: '0 57 16 16' },
                errorMessageElement: { xywh: '21 57 149 16' },
                labelHtmlElement: { xywh: '0 79 50 17'}
            });
        });
    });

    describe('fit layout', function () {
        it('should grow', function () {
            create({
                label: null
            }, {
                layout: 'fit',
                height: 150,
                width: 300
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 300 150' },
                labelElement: { d: false },
                bodyElement: { xywh: '0 0 300 150' },
                inputElement: { xywh: '1 1 298 148' },
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
                height: 50,
                width: 100
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 100 50' },
                labelElement: { d: false },
                bodyElement: { xywh: '0 0 100 50' },
                inputElement: { xywh: '1 1 98 48' },
                errorElement: { d: false },
                errorIconElement: { d: false },
                errorMessageElement: { d: false }
            });
        });
    });
});
