type ReturnFn = (args?: any[]) => void;
type TimeoutFn = <T>(fn: T, wait: number) => ReturnFn;

const debounce: TimeoutFn = <T>(fn: T, wait: number) => {
  let timer: NodeJS.Timeout | undefined;
  return (...args: any[]) => {
    clearTimeout(timer);
    timer = setTimeout(() => {
      typeof fn === "function" && fn(...args);
    }, wait);
  };
};

const throttle: TimeoutFn = <T>(fn: T, wait: number) => {
  let timer: NodeJS.Timeout | undefined;
  return (...args: any[]): void => {
    if (timer) return;
    timer = setTimeout(() => {
      typeof fn === "function" && fn(...args);
      timer = undefined;
    }, wait);
  };
};

type Func<T extends any[], R> = (...args: T) => R;

const curry = <T extends any[], R>(fn: Func<T, R>): Func<T, R> => {
  const arity = fn.length;

  return function curried(...args: any[]): any {
    if (args.length >= arity) {
      return fn.apply(this, args);
    } else {
      return (...nextArgs: any[]) => {
        return curried.apply(this, args.concat(nextArgs));
      };
    }
  } as Func<T, R>;
};

export { curry, debounce, throttle };
