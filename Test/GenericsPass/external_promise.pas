type TOnFulfilled<T> = procedure(response: T); 
type TOnRejected = procedure(reason: Variant); 

type JPromise<T> = class external 'Promise'
    constructor Create(Executor: procedure(resolve: TOnFulFilled<T>; reject: TOnRejected));
    class function all(iterable: array of JPromise<T>): JPromise<T>;
    class function race(iterable: array of JPromise<T>): JPromise<T>;
    class function reject(reason: Variant): JPromise<T>;
    class function resolve(value: T): JPromise<T>;

    function &then(onFulfilled: TOnFulfilled<T>; onRejected: TOnRejected): JPromise<T>;
    procedure catch(onRejected: TOnRejected);
end;

var p := JPromise<Integer>;