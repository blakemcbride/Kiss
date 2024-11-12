/**
 * A class representing a mutable string, allowing character modifications,
 * insertions, deletions, replacements, appending, and removing from both the start and end.
 */
class MutableString {
    /**
     * Initializes a new instance of the MutableString class.
     * @param {string} initialString - The initial string to use as the basis for the mutable string.
     */
    constructor(initialString = "") {
        this.characters = Array.from(initialString);
    }

    /**
     * Sets a character at a specific index.
     * @param {number} index - The position to set the character.
     * @param {string} char - The character to set. Only the first character is used if a string is passed.
     */
    setCharAt(index, char) {
        if (index >= 0 && index < this.characters.length && char.length > 0)
            this.characters[index] = char[0];
    }

    /**
     * Inserts a character or string at a specified index.
     * @param {number} index - The position to insert the character or string.
     * @param {string} str - The string to insert at the given index.
     */
    insert(index, str) {
        if (index >= 0 && index <= this.characters.length)
            this.characters.splice(index, 0, ...str);
    }

    /**
     * Deletes a character at a specified index.
     * @param {number} index - The position of the character to delete.
     */
    delete(index) {
        if (index >= 0 && index < this.characters.length)
            this.characters.splice(index, 1);
    }

    /**
     * Replaces all occurrences of a substring with a new string.
     * @param {string} target - The substring to replace.
     * @param {string} replacement - The string to replace the target substring with.
     */
    replace(target, replacement) {
        let str = this.toString();
        this.characters = Array.from(str.split(target).join(replacement));
    }

    /**
     * Clears all characters from the mutable string by truncating the array.
     */
    clear() {
        this.characters.length = 0;
    }

    /**
     * Gets the character at a specified index.
     * @param {number} index - The position of the character to retrieve.
     * @returns {string} - The character at the specified index.
     */
    charAt(index) {
        if (index >= 0 && index < this.characters.length)
            return this.characters[index];
        return '';
    }

    /**
     * Returns the current length of the mutable string.
     * @returns {number} - The length of the mutable string.
     */
    length() {
        return this.characters.length;
    }

    /**
     * Appends a string to the end of the mutable string.
     * @param {string} str - The string to append to the mutable string.
     */
    append(str) {
        this.characters.push(...str);
    }

    /**
     * Prepends a string to the beginning of the mutable string.
     * @param {string} str - The string to prepend to the mutable string.
     */
    prepend(str) {
        this.characters.unshift(...str);
    }

    /**
     * Removes the first n characters from the mutable string.
     * @param {number} n - The number of characters to remove from the beginning.
     */
    removeFromStart(n) {
        if (n > 0 && n <= this.characters.length)
            this.characters.splice(0, n);
    }

    /**
     * Removes the last n characters from the mutable string.
     * @param {number} n - The number of characters to remove from the end.
     */
    removeFromEnd(n) {
        if (n > 0 && n <= this.characters.length)
            this.characters.splice(this.characters.length - n, n);
    }

    /**
     * Converts the mutable string back to a regular string.
     * @returns {string} - The current string value.
     */
    toString() {
        return this.characters.join("");
    }
}
