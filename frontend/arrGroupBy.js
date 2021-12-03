// function way
function arrayGroupBy(list, groupId) {
  function groupBy(array, func) {
    const groups = {};
    array.forEach(function (elem) {
      const group = JSON.stringify(func(elem));
      groups[group] = groups[group] || [];
      groups[group].push(elem);
    });
    return groups;
  }
  return groupBy(list, function (item) {
    return item[groupId];
  });
}

// ES6
[...new Set(items.map(elem => elem.field))].forEach(elem =>
  this.storeArray.push(items.filter(item => item.field === elem)))
