type callbackFn = (args?: unknown[]) => void;
type TimeoutFn<T extends callbackFn = callbackFn> = (
  callback: T,
  delay: number,
) => (...args: Parameters<typeof callback>) => void;

const debounce: TimeoutFn = (callback, delay) => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => {
      typeof callback === "function" && callback(...args);
    }, delay);
  };
};

const throttle: TimeoutFn = (callback, delay) => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  return (...args): void => {
    if (timer) return;
    timer = setTimeout(() => {
      typeof callback === "function" && callback(...args);
      timer = undefined;
    }, delay);
  };
};

type Fn<T extends unknown[], R> = (...args: T) => R;

const curry = <T extends unknown[], R>(fn: Fn<T, R>): Fn<T, R> => {
  const arity = fn.length;
  return function curried(...args: any[]): any {
    if (args.length >= arity) {
      return fn.apply(this, args);
    } else {
      return (...nextArgs: any[]) => {
        return curried.apply(this, args.concat(nextArgs));
      };
    }
  } as Fn<T, R>;
};

export { curry, debounce, throttle };
