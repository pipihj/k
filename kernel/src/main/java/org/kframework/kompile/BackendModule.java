// Copyright (c) Runtime Verification, Inc. All Rights Reserved.
package org.kframework.kompile;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.multibindings.MapBinder;
import java.util.Map;
import org.kframework.backend.Backend;
import org.kframework.backend.kore.KoreBackend;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;

public class BackendModule extends AbstractModule {
  @Override
  protected void configure() {
    binder().requireAtInjectOnConstructors();
    MapBinder<String, Backend> backendBinder =
        MapBinder.newMapBinder(binder(), String.class, Backend.class);
    backendBinder.addBinding("kore").to(KoreBackend.class);
  }

  @Provides
  Backend getKoreBackend(KompileOptions options, Map<String, Backend> map, KExceptionManager kem) {
    Backend backend = map.get(options.backend);
    if (backend == null) {
      throw KEMException.criticalError(
          "Invalid backend: " + options.backend + ". It should be one of " + map.keySet());
    }
    return backend;
  }
}
