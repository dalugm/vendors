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

export { debounce, throttle };
