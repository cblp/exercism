use std::collections::HashMap;

type InternalCellId = usize;
type ComputeFn<T> = Box<dyn Fn(&[T]) -> T>;
type Callbacks<'a, T> = HashMap<usize, Box<dyn Fn(T) + 'a>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InputCellId(InternalCellId);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ComputeCellId(InternalCellId);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallbackId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CellId {
    Input(InputCellId),
    Compute(ComputeCellId),
}

impl CellId {
    fn internal(&self) -> InternalCellId {
        match self {
            CellId::Input(InputCellId(id))
            | CellId::Compute(ComputeCellId(id)) => *id,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

enum Cell<'a, T> {
    Input {
        value: T,
    },
    Compute {
        value: T,
        dependencies: Vec<InternalCellId>,
        compute_func: ComputeFn<T>,
        callbacks: Callbacks<'a, T>,
        callback_id_next: usize,
    },
}

impl<'a, T: Copy> Cell<'a, T> {
    fn value(&self) -> T {
        match self {
            Cell::Input { value } | Cell::Compute { value, .. } => *value,
        }
    }
}

pub struct Reactor<'a, T> {
    cells: Vec<Cell<'a, T>>,
}

impl<'a, T> Default for Reactor<'a, T> {
    fn default() -> Self {
        Self { cells: Vec::new() }
    }
}

impl<'a, T: Copy + PartialEq> Reactor<'a, T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates an input cell with the specified initial value,
    /// returning its ID.
    pub fn create_input(&mut self, initial: T) -> InputCellId {
        let cell_id = self.cells.len();
        self.cells.push(Cell::Input { value: initial });
        InputCellId(cell_id)
    }

    /// Creates a compute cell with the specified dependencies and compute
    /// function.
    pub fn create_compute<F: Fn(&[T]) -> T + 'static>(
        &mut self,
        dependencies: &[CellId],
        compute_func: F,
    ) -> Result<ComputeCellId, CellId> {
        let dep_values: Result<Vec<T>, CellId> = dependencies
            .iter()
            .map(|dep| {
                self.cells
                    .get(dep.internal())
                    .map(|cell| cell.value())
                    .ok_or(*dep)
            })
            .collect();
        let value = compute_func(&dep_values?);
        let cell_id = self.cells.len();
        self.cells.push(Cell::Compute {
            dependencies: dependencies.iter().map(|id| id.internal()).collect(),
            compute_func: Box::new(compute_func),
            value,
            callbacks: HashMap::new(),
            callback_id_next: 0,
        });
        Ok(ComputeCellId(cell_id))
    }

    /// Retrieves the current value of the cell, or None if the cell does not
    /// exist.
    pub fn value(&self, id: CellId) -> Option<T> {
        self.cells.get(id.internal()).map(|cell| cell.value())
    }

    /// Sets the value of the specified input cell.
    ///
    /// Returns false if the cell does not exist.
    pub fn set_value(&mut self, id: InputCellId, new_value: T) -> bool {
        if id.0 >= self.cells.len() {
            return false;
        }
        let mut is_dirty: Vec<bool> = vec![false; self.cells.len()];
        self.update_and_trigger_callbacks(id.0, &mut is_dirty, new_value);
        for i in 0..self.cells.len() {
            self.recompute(i, &mut is_dirty);
        }
        true
    }

    fn update_and_trigger_callbacks(
        &mut self,
        id: InternalCellId,
        is_dirty: &mut [bool],
        new_value: T,
    ) {
        let Some(cell) = self.cells.get_mut(id) else {
            return;
        };
        let (value, callbacks) = match cell {
            Cell::Input { value } => (value, &Callbacks::new()),
            Cell::Compute {
                value, callbacks, ..
            } => (value, callbacks as _),
        };
        if *value == new_value {
            return;
        }
        *value = new_value;
        is_dirty[id] = true;
        for cb in callbacks.values() {
            cb(new_value);
        }
    }

    fn recompute(&mut self, id: InternalCellId, is_dirty: &mut [bool]) {
        let new_value = match self.cells.get(id) {
            Some(Cell::Compute {
                dependencies,
                compute_func,
                ..
            }) if dependencies.iter().any(|&dep| is_dirty[dep]) => {
                let dep_values: Vec<T> = dependencies
                    .iter()
                    .map(|&dep| self.cells[dep].value())
                    .collect();
                compute_func(&dep_values)
            }
            _ => return,
        };
        self.update_and_trigger_callbacks(id, is_dirty, new_value);
    }

    /// Adds a callback to the specified compute cell.
    ///
    /// Returns the ID of the just-added callback, or None if the cell doesn't
    /// exist.
    pub fn add_callback<F: Fn(T) + 'a>(
        &mut self,
        id: ComputeCellId,
        callback: F,
    ) -> Option<CallbackId> {
        self.cells.get_mut(id.0).and_then(move |cell| match cell {
            Cell::Compute {
                callbacks,
                callback_id_next,
                ..
            } => {
                let callback_id = *callback_id_next;
                callbacks.insert(callback_id, Box::new(callback));
                *callback_id_next += 1;
                Some(CallbackId(callback_id))
            }
            _ => None,
        })
    }

    /// Removes the specified callback, using an ID returned from add_callback.
    ///
    /// Returns an Err if either the cell or callback does not exist.
    pub fn remove_callback(
        &mut self,
        cell: ComputeCellId,
        callback: CallbackId,
    ) -> Result<(), RemoveCallbackError> {
        self.cells
            .get_mut(cell.0)
            .ok_or(RemoveCallbackError::NonexistentCell)
            .and_then(|cell| match cell {
                Cell::Compute { callbacks, .. } => callbacks
                    .remove(&callback.0)
                    .map(|_| ())
                    .ok_or(RemoveCallbackError::NonexistentCallback),
                _ => Err(RemoveCallbackError::NonexistentCell),
            })
    }
}
