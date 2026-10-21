import isNumber from 'is-number';
import message from '@kiln/local';
if (!isNumber(42)) throw new Error('dependency failed');
console.log(message);
