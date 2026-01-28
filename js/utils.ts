type callbackFn = (args?: unknown[]) => void;
type TimeoutFn<T extends callbackFn = callbackFn> = (
  callback: T,
  delay: number,
) => (...args: Parameters<typeof callback>) => void;

export const debounce: TimeoutFn = (callback, delay) => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => {
      typeof callback === "function" && callback(...args);
    }, delay);
  };
};

export const throttle: TimeoutFn = (callback, delay) => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  return (...args): void => {
    if (timer) return;
    timer = setTimeout(() => {
      typeof callback === "function" && callback(...args);
      timer = undefined;
    }, delay);
  };
};
