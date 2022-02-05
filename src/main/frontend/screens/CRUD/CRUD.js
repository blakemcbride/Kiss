
/* global $$ */

'use strict';

(async function () {

    const WS = 'services/Crud';

    const columnDefs = [
        {headerName: 'Last Name', field: 'lastName', width: 33  },
        {headerName: 'First Name', field: 'firstName', width: 33 },
        {headerName: 'Phone Number', field: 'phoneNumber', width: 33 }
    ];
    const grid = new AGGrid('grid', columnDefs, 'id');
    grid.show();

    async function updateGrid() {
        grid.clear();
        $$('edit').disable();
        $$('delete').disable();
        const res = await Server.call(WS, 'getRecords');
        if (res._Success)
            grid.addRecords(res.rows);
    }

    updateGrid();

    $$('new').onclick(() => {
        $$('erp-title').setValue('Add Record');
        $$('erp-first-name').clear();
        $$('erp-last-name').clear();
        $$('erp-phone-number').clear();
        Utils.popup_open('edit-record-popup', 'erp-first-name');

        $$('erp-ok').onclick(async () => {
            if ($$('erp-first-name').isError('First Name'))
                return;
            if ($$('erp-last-name').isError('Last Name'))
                return;
            const data = {
                firstName: $$('erp-first-name').getValue(),
                lastName: $$('erp-last-name').getValue(),
                phoneNumber: $$('erp-phone-number').getValue()
            };
            const res = await Server.call(WS, 'addRecord', data);
            if (res._Success) {
                Utils.popup_close();
                updateGrid();
            }
        });

        $$('erp-cancel').onclick(() => {
            Utils.popup_close();
        });
    });

    function edit() {
        const row = grid.getSelectedRow();
        $$('erp-title').setValue('Edit Record');
        $$('erp-first-name').setValue(row.firstName);
        $$('erp-last-name').setValue(row.lastName);
        $$('erp-phone-number').setValue(row.phoneNumber);
        Utils.popup_open('edit-record-popup', 'erp-first-name');

        $$('erp-ok').onclick(async () => {
            if ($$('erp-first-name').isError('First Name'))
                return;
            if ($$('erp-last-name').isError('Last Name'))
                return;
            const data = {
                id: row.id,
                firstName: $$('erp-first-name').getValue(),
                lastName: $$('erp-last-name').getValue(),
                phoneNumber: $$('erp-phone-number').getValue()
            };
            const res = await Server.call(WS, 'updateRecord', data);
            if (res._Success) {
                Utils.popup_close();
                updateGrid();
            }
        });

        $$('erp-cancel').onclick(() => {
            Utils.popup_close();
        });

    }

    $$('edit').onclick(edit);
    grid.setOnRowDoubleClicked(edit);

    grid.setOnSelectionChanged((rows) => {
        $$('edit').enable(rows);
        $$('delete').enable(rows);
    });

    $$('delete').onclick(() => {
        Utils.yesNo('Conformation', 'Are you sure you want to delete the selected record?', async () => {
            const row = grid.getSelectedRow();
            const data = {
                id: row.id
            };
            const res = await Server.call(WS, 'deleteRecord', data);
            if (res._Success) {
                updateGrid();
            }
        });
    });

    $$('report').onclick(async () => {
        // This will only work if you have groff installed on your computer
        const res = await Server.call(WS, 'runReport');
        if (res._Success) {
            Utils.showReport(res.reportUrl);
        }
    });

    $$('export').onclick(async () => {
        const res = await Server.call(WS, 'runExport');
        if (res._Success) {
            Utils.showReport(res.exportUrl);
        }
    });


})();