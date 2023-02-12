package org.kissweb.database;

/**
 * Author: Blake McBride
 * Date: 2/9/23
 */
class ColumnInfo {

    private final String columnName;
    private final int dataType;
    private final int columnSize;
    private final int nullable;

    public ColumnInfo(String columnName, int dataType, int columnSize, int nullable) {
        this.columnName = columnName;
        this.dataType = dataType;
        this.columnSize = columnSize;
        this.nullable = nullable;
    }

    public String getColumnName() {
        return columnName;
    }

    public int getDataType() {
        return dataType;
    }

    public int getColumnSize() {
        return columnSize;
    }

    public int getNullable() {
        return nullable;
    }
}
